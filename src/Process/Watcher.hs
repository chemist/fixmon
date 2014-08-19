{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Process.Watcher (watcher, registerAgent, lookupAgent) where

import           Types

import           Control.Distributed.Process.Internal.Types (ProcessMonitorNotification(..), MonitorRef(..))
import           Control.Distributed.Process                         (Process, ProcessId,
                                                                      say)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform
import Control.Applicative  ((<$>))
import Data.Bimap
import Prelude hiding (lookup)

---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

registerAgent :: ProcessId -> (Hostname, ProcessId, ProcessId) -> Process Bool
registerAgent = call 

lookupAgent :: Hostname -> Process (Maybe ProcessId)
lookupAgent = call (Registered "watcher")

watcher :: Process ()
watcher = serve () initServer server

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

defDelay :: Delay
defDelay = Delay $ seconds 20

data AgentInfo = AgentInfo MonitorRef ProcessId ProcessId deriving (Show)

instance Eq AgentInfo where
  AgentInfo x y _ == AgentInfo x1 y1 _ = x == x1 && y == y1

instance Ord AgentInfo where
    compare (AgentInfo x y _) (AgentInfo x1 y1 _) = compare (x, y) (x1, y1)

getPid :: AgentInfo -> ProcessId
getPid (AgentInfo _ _ p) = p

type ST = Bimap Hostname AgentInfo

initServer :: InitHandler () ST
initServer _ = do
    say "start watcher"
    return $ InitOk empty defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ registerNew, searchAgent ]
    , infoHandlers = [catchDead]
    }


registerNew :: Dispatcher ST
registerNew = handleCall $ \st (Hostname t, p, c) -> do
    say "register new agent"
    m <- monitor p
    maybe (reply False st) (\x -> reply True (insert (Hostname t) (AgentInfo x p c) st)) m

searchAgent :: Dispatcher ST
searchAgent = handleCall $ \st (h :: Hostname) -> reply (getPid <$> (lookup h st :: Maybe AgentInfo)) st

catchDead :: DeferredDispatcher ST
catchDead = handleInfo $ \st (ProcessMonitorNotification x y whyDead) -> do
    say "catch dead"
    say $ show whyDead
    let newSt = deleteR (AgentInfo x y undefined) st
    say $ show newSt
    continue $ newSt


