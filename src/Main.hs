module Main where

-- import           Types
import           Process.Configurator
import           Process.Cron
import           Process.Tasker

import           Control.Applicative
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad.State
import           Network.Transport                (closeTransport)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)
-- import Control.Exception (SomeException)

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        void . spawnLocal $ cron
        void . spawnLocal $ store
        void . spawnLocal $ clock
        void . spawnLocal $ tasker
        void . spawnLocal $ supervisor
        say . show =<< getSelfPid
        _ <- liftIO $ readLn :: Process String
        return ()
        -- replLoop
    closeLocalNode node
    closeTransport t

supervisor :: Process ()
supervisor = do
    say "start supervisor"
    Just cronPid <- whereis "cron"
    Just storePid <- whereis "configurator"
    Just clockPid <- whereis "clock"
    Just taskerPid <- whereis "tasker"
    void . monitor $ cronPid
    void . monitor $ storePid
    void . monitor $ clockPid
    void . monitor $ taskerPid
    names <- zip [cronPid, storePid, clockPid, taskerPid] <$> sequence [getProcessInfo cronPid, getProcessInfo storePid, getProcessInfo clockPid, getProcessInfo taskerPid]
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
