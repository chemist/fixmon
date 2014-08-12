{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Process.Watcher (watcher, hello, lookupAgent) where

import           Types
import Process.Checker (doTask)
import Check.System

import           Control.Distributed.Process.Internal.Types (ProcessMonitorNotification(..), MonitorRef(..))
import           Control.Distributed.Process                         (Process, ProcessId,
                                                                      processNodeId,
                                                                      say)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform
-- import           Data.Map                                            (Map, empty, insert, lookup)
import Control.Applicative  ((<$>))
import Data.Bimap
import Prelude hiding (lookup)

hello :: ProcessId -> (Hostname, ProcessId) -> Process Bool
hello = call 

lookupAgent :: ProcessId -> Hostname -> Process (Maybe ProcessId)
lookupAgent = call 

defDelay :: Delay
defDelay = Delay $ seconds 20

watcher :: Process ()
watcher = serve () initServer server

data AgentInfo = AgentInfo MonitorRef ProcessId deriving (Eq, Ord, Show)

getPid :: AgentInfo -> ProcessId
getPid (AgentInfo _ p) = p

type ST = Bimap Hostname AgentInfo

initServer :: InitHandler () ST
initServer _ = do
    say "start watcher"
--    register "cron" =<< getSelfPid
    return $ InitOk empty defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ registerNew, searchAgent ]
    , infoHandlers = [catchDead]
    }


registerNew :: Dispatcher ST
registerNew = handleCall $ \st (Hostname t, p) -> do
    say "register new agent"
    m <- monitor p
    say $ show m
    let ni = processNodeId p
    say $ show ni
    h <- resolve (Registered "checker" )
    say $ show h
--    h <- doTask (RemoteRegistered "checker" ni) testHostname
--    say $ show m
--    say $ "hostname check result " ++ show h
    maybe (reply False st) (\x -> reply True (insert (Hostname t) (AgentInfo x p) st)) m

searchAgent :: Dispatcher ST
searchAgent = handleCall $ \st (h :: Hostname) -> do
    say "search agent"
    reply (getPid <$> (lookup h st :: Maybe AgentInfo)) st

catchDead :: DeferredDispatcher ST
catchDead = handleInfo $ \st (ProcessMonitorNotification x y whyDead) -> do
    say "catch dead"
    say $ show whyDead
    continue $ deleteR (AgentInfo x y) st


