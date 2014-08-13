{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Process.Agent
( agent
)
where


import           Control.Distributed.Process hiding (monitor)                       
import           Control.Distributed.Process.Platform                (Recipient (..), resolve, monitor)
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform.Supervisor
import           Control.Distributed.Process.Node
import           Network.Transport                (EndPointAddress (..),
                                                   closeTransport)
import           Data.Time.Clock
import Data.ByteString (ByteString)
import           Data.Typeable                                       (Typeable)
import           GHC.Generics                                        (Generic)
import           Data.Binary
import Process.Watcher (registerAgent)
import Types

---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

agent :: LocalHost -> Server -> Process ()
agent localHostname remoteAddress = serve (localHostname, remoteAddress) initServer server

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

tryFound :: Process ()
tryFound = cast (Registered "registerAgent") PingServer

defDelay :: Delay
defDelay = Delay $ seconds 1

type LocalHost = Hostname
type Server = ByteString

data ST = New LocalHost Server
        | Founded LocalHost Server ProcessId
        | RegisteredInServer LocalHost Server ProcessId MonitorRef
        deriving Show

initServer :: InitHandler (LocalHost, Server) ST
initServer (hostname, server) = do
    say "start register agent"
    return $ InitOk (New hostname server) defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ foundServer ]
    , timeoutHandler = \s _ -> do
        tryFound
        timeoutAfter_ defDelay s
    , infoHandlers = [catchRegister, catchDeadServer]
    }

data PingServer = PingServer deriving (Typeable, Generic)
instance Binary PingServer


catchRegister :: DeferredDispatcher ST
catchRegister = handleInfo $ \st (WhereIsReply _ m) ->
  case (st, m) of
       (New l s, Just x) -> do
           say "reply from server"
           say $ show x
           continue (Founded l s x)
       _     -> continue st

catchDeadServer :: DeferredDispatcher ST
catchDeadServer = handleInfo $ \(RegisteredInServer l s p m) (x :: ProcessMonitorNotification) -> do
    say "Zed dead baby!!!"
    say $ show x
    continue $ New l s

foundServer :: Dispatcher ST
foundServer = handleCast $ \st PingServer -> case st of
         New l s -> do
            whereisRemoteAsync (NodeId (EndPointAddress s)) "watcher"
            say "try register"
            continue st
         Founded l s pid -> do
             self <- getSelfPid
             checkerP <- resolve (Registered "checker")
             mm <- monitor pid
             case (checkerP, mm) of
                  (Just cp, Just m) -> do
                      r <- registerAgent pid (l, self, cp)
                      if r 
                         then continue (RegisteredInServer l s pid m)
                         else continue (Founded l s pid)
                  _ -> continue (Founded l s pid)
         _ -> continue st

