module Process.Logger 
( logger
, notice
, warning
, critical
)
where

import Control.Distributed.Process
import Control.Distributed.Process.Platform
import Control.Distributed.Process.Platform.Time
import Control.Distributed.Process.Platform.Timer
import Control.Distributed.Process.Platform.ManagedProcess
import qualified Control.Distributed.Process.Platform.Service.SystemLog as L

logger :: L.LogLevel -> Process ProcessId
logger l = L.systemLogFile "fixmon.log" l return

notice :: String -> Process ()
notice msg = do
    Just cl <- L.client
    L.notice cl (L.LogText msg)

warning :: String -> Process ()
warning msg = do
    Just cl <- L.client
    L.warning cl (L.LogText msg)

critical :: String -> Process ()
critical msg = do
    Just cl <- L.client
    L.critical cl (L.LogText msg)

