{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
module Process.TaskPool where

import Control.Distributed.Process.Platform.Task
import Control.Distributed.Process
import Control.Distributed.Process.Platform
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Closure
import Data.Typeable
import Control.Exception hiding (try)

import Types
import           Process.Checker                                     (doTask)
import           Process.Watcher                                     (lookupAgent)


task :: (Hostname, Check, CheckHost) -> Process ()
task (host, check, ch) = do
    say "task"
    pidAgent <- lookupAgent host
    say "get pid"
    say $ "get pid" ++ show pidAgent
    case pidAgent of
         Just pid -> do
             dt <- doTask (Pid pid) check
             say $ "for host " ++ show host ++ " check " ++ show check ++ " result " ++ show dt
         Nothing -> say $ "host " ++ show host ++ " not found"

$(remotable ['task])

poolT :: Process (InitResult (BlockingQueue (Hostname, Check, CheckHost)))
poolT = pool 10

taskPoolName :: Recipient
taskPoolName = Registered "pool"

taskPool :: Process ()
taskPool = start poolT

addTask :: (Hostname, Check, CheckHost) -> Process ()
addTask x = do
    say "add task"
    Just r <- resolve taskPoolName
    say $ "pid " ++ show r
    e <- try $ executeTask r ($(mkClosure 'task) x)
    say $ show (e :: Either SomeException (Either ExitReason ()))


