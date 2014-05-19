{-# LANGUAGE DeriveDataTypeable         #-}
module Main where

import           Process.Configurator
import           Process.Cron
import           Process.Tasker
import           Process.Web
import Types

import           Control.Applicative hiding (empty)
import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad.Reader             (ask)
import           Control.Monad.State
import           Network.Transport                (closeTransport, newEndPoint, receive, EndPoint, ConnectionId, Connection, connect, Event(..), defaultConnectHints)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)
                                                  
import qualified Network.Transport as NT
import Control.Concurrent.MVar
import Data.Map (Map, empty, insert, (!), delete)
import qualified Data.Binary as B
import Data.Typeable


main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        s <-  ask
        void . spawnLocal $ do
            register "web" =<< getSelfPid
            say "start web"
            liftIO $ web s
        void . spawnLocal $ cron
        void . spawnLocal $ store
        void . spawnLocal $ clock
        void . spawnLocal $ tasker
        void . spawnLocal $ supervisor
        void . spawnLocal $ registrator
        say . show =<< getSelfPid
        _ <- liftIO getLine :: Process String
        say "kill web"
        maybe (return ()) (`kill` "stop web") =<< whereis "web"
        return ()
        -- replLoop
    closeLocalNode node
    closeTransport t

registrator :: Process ()
registrator = do
    p <- getSelfPid
    register "registrator" p
    say "start registrator"
    say $ "registrator " ++ " pid " ++ show p
    go
    where 
      go = forever $ do
              remoteAgentPid <-  expectTimeout 100000 :: Process (Maybe ProcessId)
              maybe (return ()) (\p -> p `send` Reg >> p `send` testHttp) remoteAgentPid
--               say $ "echo get " ++ show remoteAgentPid
--

data Reg = Reg deriving (Show, Typeable)

instance B.Binary Reg where
    put _ = B.put (0::Int)
    get = do
        x <- B.get :: B.Get Int
        return Reg


supervisor :: Process ()
supervisor = do
    say "start supervisor"
    Just cronPid <- whereis "cron"
    Just storePid <- whereis "configurator"
    Just clockPid <- whereis "clock"
    Just taskerPid <- whereis "tasker"
    Just webPid <- whereis "web"
    void . monitor $ cronPid
    void . monitor $ storePid
    void . monitor $ clockPid
    void . monitor $ taskerPid
    void . monitor $ webPid
    names <- zip [cronPid, storePid, clockPid, taskerPid, webPid] <$> sequence [getProcessInfo cronPid, getProcessInfo storePid, getProcessInfo clockPid, getProcessInfo taskerPid, getProcessInfo webPid]
    forever $ do
        (ProcessMonitorNotification _ pid _) <- expect :: Process ProcessMonitorNotification
        say "supervisor <- (ProcessMonitorNotification)"
        say . show $ lookup pid names
{--
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

--}
