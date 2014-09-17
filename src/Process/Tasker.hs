{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Process.Tasker
( tasker
, doTasks
) where


import           Control.DeepSeq
-- import           Control.Monad                                       (void)
import           Process.Checker                                     (doTask)
import           Process.Configurator                                (Update (..),
                                                                      checkById, hostById, triggerById)
import           Process.Storage                                     (saveResult, checkTrigger)
import           Process.Watcher                                     (lookupAgent)
import           Types

import           Control.Distributed.Process                         hiding
                                                                      (call)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
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
    makeCheck _ _ Nothing Nothing = critical "not found any triggers" -- TODO Alarm here!!
    makeCheck (Just host) _ (Just trigger) Nothing  = do
        checkTrigger (host, trigger)
        critical "TODO Alarm " 
    makeCheck (Just host) (Just check) (Just trigger) (Just pid) = do
        dt <- doTask (Pid pid) check
        saveResult (host, dt)
        checkTrigger (host, trigger)
    makeCheck (Just host) (Just check) Nothing (Just pid) = do
        dt <- doTask (Pid pid) check
        saveResult (host, dt)
    makeCheck _ _ _ _ = warning "Upps!!!! check or host not found, bug here"

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
    warning "start tasker"
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


