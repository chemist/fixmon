{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Process.Checker
where


import           Types
import           Check.System
import           Check.Http

import           Control.Distributed.Process hiding (try, call)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time

import Control.Exception
import Data.Map (empty, lookup, insert, unions, fromList)
import           Prelude                                             hiding
                                                                      (lookup)

checker :: Process ()
checker = serve () initServer server

checkerName :: Recipient
checkerName = Registered "checker"

doTask :: Recipient -> Check -> Process Complex
doTask = call

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
    { apiHandlers = [ taskDispatcher ]
    , infoHandlers = []
    }

taskDispatcher :: Dispatcher Route
taskDispatcher = handleCall $ \st (check :: Check) -> do
    let ch = lookup (ctype check) st
    maybe (notFound st) (doCheck st check) ch
    where
      notFound = reply (Complex $ fromList [("_status_", Any $ Bool False)])
      doCheck st check doCheck = do
          checkResult <- liftIO $ try $ doCheck check
          case checkResult of
               Left (e :: SomeException) -> reply (Complex $ fromList [("_status_", Any $ Bool False)]) st
               Right r -> reply r st

