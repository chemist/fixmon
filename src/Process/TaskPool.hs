{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE BangPatterns     #-}
module Process.TaskPool
( addTask
, taskPool
, __remoteTable
)
where

import           Control.Distributed.Process                         (Process, spawnLocal,
                                                                      say)
import           Control.Distributed.Process.Closure                 (mkClosure,
                                                                      remotable)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Task           (BlockingQueue, executeTask, stats, BlockingQueueStats(..),
                                                                      pool,
                                                                      start, )
import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform.Timer
import           Control.Distributed.Process.Platform.Async
import           Control.Distributed.Process.Serializable            ()

import           Process.Checker                                     (doTask)
import           Process.Watcher                                     (lookupAgent)
import           Types
import Control.Monad (void)
import Control.Applicative

taskmake :: (Hostname, Check, CheckHost) -> Process ()
taskmake (host, check, ch) = do
    pidAgent <- lookupAgent host
--     sleep $ seconds 5
    case pidAgent of
         Just pid -> do
             dt <- doTask (Pid pid) check
             say $ "for host " ++ show host ++ " check " ++ show check ++ " result " ++ show dt
         Nothing -> say $ "host " ++ show host ++ " not found"

$(remotable ['taskmake])


addTask :: (Hostname, Check, CheckHost) -> Process ()
addTask x = do
    poolStatus <-  (stats . Registered) "pool"
    say $ show $ activeJobs <$> poolStatus
    say $ show $ queuedJobs <$> poolStatus
    job <- return $ ($(mkClosure 'taskmake) (x :: (Hostname,Check, CheckHost)) )
    spawnLocal $ void $ executeTask taskPoolName job
    return ()

poolT :: Process (InitResult (BlockingQueue ()))
poolT = pool 100

taskPoolName :: Recipient
taskPoolName = Registered "pool"

taskPool :: Process ()
taskPool = start poolT

