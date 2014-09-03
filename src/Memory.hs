{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables         #-}
module Main where

import           Control.Concurrent                              (threadDelay)
import           Control.Distributed.Process hiding (call)
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Platform
import           Control.Distributed.Process.Platform.Supervisor
import           Control.Monad
import           Data.Binary
import qualified Data.Binary                                     as B
import           Data.ByteString                                 (ByteString)
import           Data.Map.Strict                                        (empty, (!), fromList)
import           Data.Typeable hiding (cast)
import           Network.Transport                               (EndPointAddress (..), closeTransport)
import           Network.Transport.TCP                           (createTransport, defaultTCPParameters)
import           Control.Distributed.Process.Platform.ManagedProcess hiding (shutdown, runNode, runProcess)
import           Control.Distributed.Process.Platform.Time
import GHC.Generics
import Data.Text hiding (map, zip)
import           Data.Text.Binary    ()
import Types
import Checks
import Check.System
import qualified Data.Map.Strict as Map
import System.Environment
import qualified Process.Watcher as S
import qualified Process.Agent as A
import           Control.Exception (SomeException)

host, portServer, portAgent :: String
host = "127.0.0.1"
portServer = "10501"
portAgent = "10503"

remoteAddress :: ByteString
remoteAddress = "127.0.0.1:10501:0"

localhost = Hostname "localhost"

defDelay :: Delay
defDelay = Delay $ milliSeconds 10

main :: IO ()
main = do
    args <- getArgs
    case args of
         ["server"] -> server
         ["agent"] -> agent

server :: IO ()
server = do
    Right t <- createTransport host portServer defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        cstart <-  mapM toChildStart [one, S.watcher]
        let cspec = map child $ zip cstart ["one", "watcher"]
        superPid <- super cspec
        _ <- liftIO $ getLine :: Process String
        say "kill super"
        shutdown (Pid superPid)
        return ()
    closeLocalNode node
    closeTransport t

agent :: IO ()
agent = do
    Right t <- createTransport host portAgent defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        cstart <-  mapM toChildStart [two, A.agent localhost remoteAddress]
        let cspec = map child $ zip cstart ["checker", "agent"]
        superPid <- super cspec
        _ <- liftIO $ getLine :: Process String
        say "kill super"
        shutdown (Pid superPid)
        return ()
    closeLocalNode node
    closeTransport t


super :: [ChildSpec] -> Process SupervisorPid
super = start restartOne ParallelShutdown

child :: (ChildStart, String) -> ChildSpec
child (chStart, who) =  ChildSpec who Worker Permanent TerminateImmediately chStart (Just $ LocalName who)

one :: Process ()
one = serve 0 (\x -> return $ InitOk x defDelay) server
  where
  server = defaultProcess
    { apiHandlers = []
    , timeoutHandler = \st _ -> do
        mpid <- S.lookupAgent $ Hostname "localhost"
        case mpid of
             Just pid -> do
                 say "send ping"
                 doMany pid
             Nothing -> say "agent not found"
        timeoutAfter_ defDelay st
    }

doMany :: ProcessId -> Process ()
doMany pid = mapM_ fun [testHostname, testUptime, testBootTime, testCpuInfo]
  where
  fun check = do
      r <- ping (Pid pid) check
      say $ "requeest " ++ show r

    
two :: Process ()
two = serve 0 (\x -> return $ InitOk x defDelay) server
  where
  server = defaultProcess
    { apiHandlers = [pongCall]
    }


ping :: Recipient -> Check -> Process Complex
ping = call  

pongCall :: Dispatcher Int
pongCall = handleCall $ \st (sh :: Check) -> do
    if Map.member (ctype sh) routes 
       then do
          let doCheck = routes ! (ctype sh)
          catch (good doCheck sh st) (bad st)
       else reply (Complex [("_status_", toDyn False)]) st 
    where
      good doCheck sh st = do
          r <- liftIO $ doCheck sh
          reply r (st + 1)
      bad st (_ :: SomeException) = reply (Complex [("_status_", toDyn False)]) st 

data Msg = Ping Text
         deriving (Show, Eq, Typeable, Generic)

instance Binary Msg
