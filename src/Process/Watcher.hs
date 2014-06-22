{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Process.Watcher (watcher, hello, lookupAgent) where

import           Types

import           Control.Distributed.Process                         (Process, ProcessId,
                                                                      liftIO,
                                                                      say)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.Binary
import           Data.Map                                            (Map, empty, insert, lookup,
                                                                      elems, filterWithKey)
import           Data.Typeable                                       (Typeable)
import           GHC.Generics                                        (Generic)
import Prelude hiding (lookup)

hello :: ProcessId -> (Hostname, ProcessId) -> Process Bool
hello = call 

lookupAgent :: ProcessId -> Hostname -> Process (Maybe ProcessId)
lookupAgent = call 

defDelay :: Delay
defDelay = Delay $ seconds 20

watcher :: Process ()
watcher = serve () initServer server


type ST = Map Hostname ProcessId

initServer :: InitHandler () ST
initServer _ = do
    say "start watcher"
--    register "cron" =<< getSelfPid
    return $ InitOk empty defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ registerNew, searchAgent ]
    , infoHandlers = []
    }


registerNew :: Dispatcher ST
registerNew = handleCall $ \st (Hostname t, p) -> do
    say "register new agent"
    reply True $ insert (Hostname t) p st

searchAgent :: Dispatcher ST
searchAgent = handleCall $ \st (h :: Hostname) -> do
    say "search agent"
    reply (lookup h st) st


