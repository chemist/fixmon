module Main where

import           Process.Configurator
import           Process.Cron
import           Process.Tasker
import           Process.Web
import           Process.Watcher
import           Process.TaskPool

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform.Supervisor
import           Control.Distributed.Process.Platform
import           Control.Distributed.Process.Node
import           Control.Monad.Reader             (ask)
import           Network.Transport                (closeTransport)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)



main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t $ Process.TaskPool.__remoteTable initRemoteTable
    runProcess node $ do
        s <-  ask
        let webProcess = do
              say "start web"
              liftIO $ web s
        cstart <-  mapM toChildStart [webProcess, configurator, tasker, taskPool, taskPool, taskPool, cron, watcher]
        let cspec = map child $ zip cstart ["web", "configurator", "tasker", "pool0", "pool1", "pool2", "cron", "watcher"]
        superPid <-  super cspec
--        c <- listChildren superPid
--        say $ show c
        _ <- liftIO getLine :: Process String
--        cc <- listChildren superPid
--        say $ show cc
--        _ <- liftIO getLine :: Process String
        say "kill super"
        shutdown (Pid superPid)
        return ()
        -- replLoop
    closeLocalNode node
    closeTransport t

super :: [ChildSpec] -> Process SupervisorPid
super = start restartOne ParallelShutdown 

child :: (ChildStart, String) -> ChildSpec
child (chStart, who) =  ChildSpec who Worker Permanent TerminateImmediately chStart (Just $ LocalName who)

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
