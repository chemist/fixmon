{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Process.RegisterAgent where


import           Control.Distributed.Process                        
import           Control.Distributed.Process.Platform                (Recipient (..))
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
import Process.Watcher (hello)
import Types

registerAgent :: Process ()
registerAgent = serve remoteAddress initServer server

defDelay :: Delay
defDelay = Delay $ seconds 1

remoteAddress :: ByteString
remoteAddress = "127.0.0.1:10501:0"

tryFound :: Process ()
tryFound = cast (Registered "registerAgent") PingServer

data ST = New ByteString
        | Founded ProcessId
        | RegisteredInServer

initServer :: InitHandler ByteString ST
initServer server = do
    say "start register agent"
    return $ InitOk (New server) defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ foundServer ]
    , timeoutHandler = \s _ -> do
        tryFound
        timeoutAfter_ defDelay s
    , infoHandlers = [handleInfo handleServerRequest]
    }

data PingServer = PingServer deriving (Typeable, Generic)
instance Binary PingServer


handleServerRequest st (WhereIsReply _ m) = 
  case (st, m) of
       (New _, Just x) -> do
           say "reply from server"
           say $ show x
           continue (Founded x)
       _     -> do
           say "skip"
           continue st

foundServer :: Dispatcher ST
foundServer = handleCast $ \st PingServer -> case st of
         New x -> do
            whereisRemoteAsync (NodeId (EndPointAddress x)) "watcher"
            say "try register"
            continue st
         Founded pid -> do
             self <- getSelfPid
             r <- hello pid (Hostname "localhost", self)
             if r
                then continue RegisteredInServer
                else continue (Founded pid)
         _ -> say "all good" >> continue st

--    whereisRemoteAsync (NodeId (EndPointAddress server)) "watcher"
--
        {--
        whereisRemoteAsync (NodeId (EndPointAddress remoteAddress)) "watcher"
        mregistrator <- expectTimeout 1000000 :: Process (Maybe WhereIsReply)
        maybe (say "watcher not found") good (mmm mregistrator)
        liftIO $ threadDelay 1000000
    -- _ <- liftIO getLine :: Process String
    closeLocalNode node
    closeTransport t
        where
          mmm :: Maybe WhereIsReply -> Maybe ProcessId
          mmm (Just (WhereIsReply _ x)) = x
          mmm _ = Nothing
          good pid = do
              say "registrator found"
              self <-  getSelfPid
              r <- hello pid (Hostname "localhost", self)
              if r
                 then say "success registered"
                 else say "cant register"
              return ()

--}





