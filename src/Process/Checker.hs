{-# LANGUAGE ScopedTypeVariables #-}
module Process.Checker
where


import           Types
import           Check.System
import           Check.Http

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time

import Data.Map (empty, lookup, insert, unions)
import           Prelude                                             hiding
                                                                      (lookup)

checker :: Process ()
checker = serve () initServer server

checkerName :: Recipient
checkerName = Registered "checker"

{-- 
doTasks :: Set CheckHost -> Process ()
doTasks = cast taskerName

--}

initServer :: InitHandler () Route
initServer _ = do
    say "start agent checker"
    let system = map route [HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime] 
        http = map route  [HttpSimple]
        shell = map route [Shell]
        all = unions $ system ++ http ++ shell
    return $ InitOk all Infinity

server :: ProcessDefinition Route
server = defaultProcess
    { apiHandlers = [ ]
    , infoHandlers = []
    }

{--
taskSet :: Dispatcher Tasker
taskSet = handleCast fun
    where
    fun :: Tasker -> Set CheckHost -> Process (ProcessAction Tasker)
    fun st x = do
        mapM_ (startCheck st) $ toList x
        say . show . toList $ x
        continue st

--}
