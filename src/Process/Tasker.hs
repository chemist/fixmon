{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Process.Tasker
( tasker
, doTasks
, taskPool
, __remoteTable
) where


import           Control.Monad                                       (void)
import           Process.Configurator                                (Update (..), getCheckMap, getHostMap, getTriggerMap, hostById, checkById)
import           Process.Checker                                     (doTask)
import           Process.Watcher                                     (lookupAgent)
import           Types

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Closure                 (mkClosure,
                                                                      remotable)
import           Control.Distributed.Process.Platform.Task           (BlockingQueue, executeTask,
                                                                      pool,
                                                                      start)
import           Data.Set                                            (Set,
                                                                      toList)
import           Data.Vector                                         (Vector,
                                                                      (!))
import           Prelude                                             hiding
                                                                      (lookup)

taskmake :: CheckHost -> Process ()
taskmake (CheckHost (hid, cid, mt)) = do
    mhost <- hostById hid
    mcheck <- checkById cid
    makeCheck mhost mcheck
    where
    makeCheck (Just host) (Just check) = do
        pidAgent <- lookupAgent host
        case pidAgent of
             Just pid -> do
                 dt <- doTask (Pid pid) check
                 say $ " result " ++ show dt
             Nothing -> say $ " not found"
    makeCheck _ _ = say "Upps!!!! check or host not found, bug here"

$(remotable ['taskmake])


addTask :: CheckHost -> Process ()
addTask x = do
--    poolStatus <-  (stats . Registered) "pool"
--    say $ show $ activeJobs <$> poolStatus
--    say $ show $ queuedJobs <$> poolStatus
    job <- return $ ($(mkClosure 'taskmake) (x :: CheckHost) )
    _ <- spawnLocal $ void $ executeTask taskPoolName job
    return ()

poolT :: Process (InitResult (BlockingQueue ()))
poolT = pool 100

taskPoolName :: Recipient
taskPoolName = Registered "pool"

taskPool :: Process ()
taskPool = start poolT

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
  , triggers :: Vector Trigger
  }

initServer :: InitHandler () Tasker
initServer _ = do
    say "start tasker"
    -- register "tasker" =<< getSelfPid
    hm <- getHostMap
    cm <- getCheckMap
    tm <- getTriggerMap
    return $ InitOk (Tasker hm cm tm) Infinity


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
    say "add task"
    addTask ch
    say "end add task"



updateConfig :: DeferredDispatcher Tasker
updateConfig = handleInfo $ \_ Update  -> do
    hm <- getHostMap
    cm <- getCheckMap
    tm <- getTriggerMap
    continue (Tasker hm cm tm)


