{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
module Gns  (module Gns) where

import           Types                         as Gns
import           GNS.Message                   as Gns
import           Process.Configurator          as Gns
import           Process.Cron

import           Control.Applicative
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad.State
import           Data.Map                         (Map, elems, keys, lookup)
import           Data.Set                         (Set)
import           Network.Transport                (closeTransport)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)
import           Prelude                          hiding (lookup)
import Control.Exception (SomeException)

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    (_, m, _) <-  readConfig
    runProcess node $ do
        void . spawnLocal $ cron m
        void . spawnLocal $ clock
        void . spawnLocal $ store
        say . show =<< getSelfPid
        replLoop
    closeLocalNode node
    closeTransport t

replLoop :: Process ()
replLoop = forever $ do
    say . show =<< getSelfPid
    pid <- getSelfPid
    inp <-  try (liftIO $ readLn) :: Process (Either SomeException Repl)
    case inp of
         Right RStop -> die "exit success"
         Right RShow -> do
             nsend "store" (GetHosts pid :: SMes Int)
             m <- expectTimeout 10000  :: Process (Maybe (SMes [Hostname]))
             say . show $ m
         Right RShowHost -> do
             nsend "store" (GetHostsId pid :: SMes Int)
             m <- expectTimeout 10000 :: Process (Maybe (SMes [HostId]))
             say . show $ m
         Right RShowCrontab -> do
             nsend "store" (GetCronMap pid :: SMes Int)
             m <- expectTimeout 10000 :: Process (Maybe (SMes (Map Cron (Set CheckHost))))
             say . show $ m
         Right (RHost x) -> do
             nsend "store" (GetHost pid x :: SMes Int)
             m <- expectTimeout 10000 :: Process (Maybe (SMes (Maybe Hostname)))
             say . show $ m
         _ -> say badInput

data Repl = RStop | RShow | RShowHost | RShowCrontab | RHost HostId | RCheck CheckId | RTrigger (TriggerId) deriving (Show, Eq, Ord, Read)

badInput :: String
badInput = unlines
  [ "bad input"
  , "Use:"
  , "\tRStop for quit"
  , "\tRShow for show state"
  , "\tRShowHost hosts for show hosts"
  ]

readConfig :: IO (Either String (), Monitoring, Log)
readConfig = runGns (StartOptions "gnc.yaml") emptyMonitoring $ do
    Gns.parseConfig

type StoreT a = StateT Monitoring Process a

store :: Process ()
store = do
    register "store" =<< getSelfPid
    (_, m, _) <- liftIO $ readConfig
    evalStateT storeT m

storeT :: StoreT ()
storeT = forever $ do
    self <-  lift getSelfPid
    m <- lift $ expect :: StoreT (SMes Int)
    case m of
         GetHosts pid -> do
             st <-  (elems . _hosts) <$> get
             lift . send pid $ SMes self st
         GetHostsId pid -> do
             st <- (keys . _hosts) <$> get
             lift . send pid $ SMes self st
         GetHost pid hid -> do
             st <- _hosts <$> get
             lift . send pid $ SMes self $ lookup hid st
         GetCheck pid hid -> do
             st <- _checks <$> get
             lift . send pid $ SMes self $ lookup hid st
--         GetTrigger pid hid -> do
--             st <- _triggers <$> get
--             lift . send pid $ SMes self $ lookup hid st
         GetCronMap pid -> do
             st <-  _periodMap <$> get
             lift . send pid $ SMes self st
         _ -> (lift . say $ "bad message to store") >> return ()


