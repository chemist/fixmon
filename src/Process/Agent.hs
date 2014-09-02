{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Agent
( agent
)
where


import           Control.Distributed.Process                         hiding
                                                                      (monitor)
import           Control.Distributed.Process.Platform                (Recipient (..),
                                                                      monitor,
                                                                      resolve)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.ByteString                                     (ByteString)
import           Network.Transport                                   (EndPointAddress (..))
import           Process.Watcher                                     (registerAgent)
import           Types

---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

agent :: LocalHost -> Server -> Process ()
agent localHostname remoteAddress = serve (localHostname, remoteAddress) initServer server

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

defDelay :: Delay
defDelay = Delay $ seconds 1

type LocalHost = Hostname
type Server = ByteString

data ST = New {-# UNPACK #-}  !LocalHost {-# UNPACK #-}  !Server
        | Founded {-# UNPACK #-}  !LocalHost {-# UNPACK #-}  !Server {-# UNPACK #-}  !ProcessId
        | RegisteredInServer {-# UNPACK #-}  !LocalHost {-# UNPACK #-}  !Server {-# UNPACK #-}  !ProcessId {-# UNPACK #-}  !MonitorRef
        deriving Show

initServer :: InitHandler (LocalHost, Server) ST
initServer (hostname, server') = do
    say "start register agent"
    return $! InitOk (New hostname server') defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = []
    , timeoutHandler = \st _ -> case st of
             New _ s -> do
                whereisRemoteAsync (NodeId (EndPointAddress s)) "watcher"
                say "try register"
                timeoutAfter_ defDelay st
             Founded l s pid -> do
                 self <- getSelfPid
                 checkerP <- resolve (Registered "checker")
                 mm <- monitor pid
                 case (checkerP, mm) of
                      (Just cp, Just m) -> do
                          r <- registerAgent pid (l, self, cp)
                          if r
                             then timeoutAfter_ defDelay (RegisteredInServer l s pid m)
                             else timeoutAfter_ defDelay (Founded l s pid)
                      _ -> timeoutAfter_ defDelay (Founded l s pid)
             _ -> timeoutAfter_ defDelay st
    , infoHandlers = [catchRegister, catchDeadServer]
    , unhandledMessagePolicy = Log
    }

catchRegister :: DeferredDispatcher ST
catchRegister = handleInfo $ \st (WhereIsReply _ m) ->
  case (st, m) of
       (New l s, Just x) -> do
           say "reply from server"
           say $ show x
           continue (Founded l s x)
       _     -> continue st

catchDeadServer :: DeferredDispatcher ST
catchDeadServer = handleInfo $ \(RegisteredInServer l s _ _) (x :: ProcessMonitorNotification) -> do
    say "Zed dead baby!!!"
    say $ show x
    continue $ New l s

