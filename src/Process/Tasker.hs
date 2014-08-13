{-# LANGUAGE ScopedTypeVariables #-}
module Process.Tasker
( tasker
, doTasks
) where


import           Process.Checker                                     (doTask)
import           Process.Configurator                                (Update (..), getCheckMap, getHostMap)
import           Process.Watcher                                     (lookupAgent)
import           Types

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.Set                                            (Set,
                                                                      toList)
import           Data.Vector                                         (Vector,
                                                                      (!))
import           Prelude                                             hiding
                                                                      (lookup)

---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

tasker :: Process ()
tasker = serve () initServer server

doTasks :: Set CheckHost -> Process ()
doTasks = cast taskerName

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------
taskerName :: Recipient
taskerName = Registered "tasker"

data Tasker = Tasker
  { hosts  :: Vector Hostname
  , checks :: Vector Check
  }

initServer :: InitHandler () Tasker
initServer _ = do
    say "start tasker"
    -- register "tasker" =<< getSelfPid
    hm <- getHostMap
    cm <- getCheckMap
    return $ InitOk (Tasker hm cm) Infinity


server :: ProcessDefinition Tasker
server = defaultProcess
    { apiHandlers = [ taskSet
                    ]
    , infoHandlers = [updateConfig]
    }

taskSet :: Dispatcher Tasker
taskSet = handleCast fun
    where
    fun :: Tasker -> Set CheckHost -> Process (ProcessAction Tasker)
    fun st x = do
        mapM_ (startCheck st) $ toList x
        continue st

startCheck :: Tasker -> CheckHost -> Process ()
startCheck st ch = do
    let host'' = hosts st ! hostN ch
        check'' = checks st ! checkN ch
    pidAgent <- lookupAgent host''
    case pidAgent of
         Just pid -> do
             dt <- doTask (Pid pid) check''
             say $ "for host " ++ show host'' ++ " check " ++ show check'' ++ " result " ++ show dt
         Nothing -> say $ "host " ++ show host'' ++ " not found"
    where
        hostN :: CheckHost -> Int
        hostN (CheckHost (i, _)) = unId i

        checkN :: CheckHost -> Int
        checkN (CheckHost (_, i)) = unId i



updateConfig :: DeferredDispatcher Tasker
updateConfig = handleInfo $ \_ Update  -> do
    hm <- getHostMap
    cm <- getCheckMap
    continue (Tasker hm cm)
