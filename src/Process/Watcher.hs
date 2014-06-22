{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Process.Watcher (watcher, hello, lookupAgent) where

import           Types

import           Control.Distributed.Process.Internal.Types (ProcessMonitorNotification(..), MonitorRef(..))
import           Control.Distributed.Process                         (Process, ProcessId,
                                                                      monitor, 
                                                                      say)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.Map                                            (Map, empty, insert, lookup)
import Prelude hiding (lookup)

hello :: ProcessId -> (Hostname, ProcessId) -> Process Bool
hello = call 

lookupAgent :: ProcessId -> Hostname -> Process (Maybe ProcessId)
lookupAgent = call 

defDelay :: Delay
defDelay = Delay $ seconds 20

watcher :: Process ()
watcher = serve () initServer server


type ST = Map Hostname MonitorRef

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
    reply True $ insert (Hostname t) m st

searchAgent :: Dispatcher ST
searchAgent = handleCall $ \st (h :: Hostname) -> do
    say "search agent"
    reply (lookup h st) st

catchDead :: DeferredDispatcher ST
catchDead = handleInfo $ \st (x :: ProcessMonitorNotification ) -> do
    say "catch dead"
    say $ show x
    continue st


