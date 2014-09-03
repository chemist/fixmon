{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Process.Tasker
( tasker
, doTasks
, taskPool
, __remoteTable
) where


import           Control.DeepSeq
import           Control.Monad                                       (void)
import           Process.Checker                                     (doTask)
import           Process.Configurator                                (Update (..),
                                                                      checkById, hostById, triggerById)
import           Process.Storage                                     (saveResult)
import           Process.Watcher                                     (lookupAgent)
import           Types

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Closure                 (mkClosure,
                                                                      remotable)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Task           (BlockingQueue, executeTask,
                                                                      pool,
                                                                      start)
import           Control.Distributed.Process.Platform.Time
import           Data.Set                                            (Set,
                                                                      toList)
import           Prelude                                             hiding
                                                                      (lookup)

taskmake :: CheckHost -> Process ()
taskmake (CheckHost (hid, cid, mt)) = do
    mhost <- hostById hid
    mcheck <- checkById cid
    mtrigger <-  maybe (return Nothing) triggerById mt
    mpid <- maybe (return Nothing) lookupAgent mhost
    makeCheck mhost mcheck mtrigger mpid
    where
    makeCheck _ _ Nothing Nothing = return ()
    makeCheck _ _ _ Nothing  = return ()
    makeCheck (Just host) (Just check) (Just trigger) (Just pid) = do
        dt <- doTask (Pid pid) check
        saveResult (host, dt)
--         !_ <- liftIO $! eval Env dt (tresult trigger)
        return ()
    makeCheck (Just host) (Just check) Nothing (Just pid) = do
        dt <- doTask (Pid pid) check
        saveResult (host, dt)
    makeCheck _ _ _ _ = say "Upps!!!! check or host not found, bug here"

$(remotable ['taskmake])


addTask :: CheckHost -> Process ()
addTask x = do
--    poolStatus <-  (stats . Registered) "pool"
--    say $ show $ activeJobs <$> poolStatus
--    say $ show $ queuedJobs <$> poolStatus
    !job <- return $! ($(mkClosure 'taskmake) (x :: CheckHost) )
    !_ <- spawnLocal $ void $! executeTask taskPoolName job
    return ()

poolT :: Process (InitResult (BlockingQueue ()))
poolT = pool 1

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

{--
data Tasker = Tasker
  { hosts    :: !(Vector Hostname)
  , checks   :: !(Vector Check)
  , triggers :: !(Vector Trigger)
  }
  --}

initServer :: InitHandler () ()
initServer _ = do
    say "start tasker"
    -- register "tasker" =<< getSelfPid
    return $! InitOk () Infinity


server :: ProcessDefinition ()
server = statelessProcess
    { apiHandlers = [ taskSet
                    ]
    , infoHandlers = [updateConfig]
    , unhandledMessagePolicy = Log
    }

taskSet :: Dispatcher ()
taskSet = handleCast_ fun
    where
    fun :: Set CheckHost -> () -> Process (ProcessAction ())
    fun x _ = do 
        mapM_ taskmake (force $ toList x)
        continue_ ()

updateConfig :: DeferredDispatcher ()
updateConfig = handleInfo $ \_ Update  -> do
    continue $! ()


