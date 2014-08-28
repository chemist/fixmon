{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main where

import           Check.Http
import           Check.System                                    ()
import           Types                                           (Check (..), CheckName (..), Checkable (..),
                                                                  Cron (..),
                                                                  Hostname (..))

import           Control.Concurrent                              (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Platform
import           Control.Distributed.Process.Platform.Supervisor
import           Control.Monad
import qualified Control.Monad.State                             as ST
import           Control.Monad.Trans.Class
import           Data.Binary
import qualified Data.Binary                                     as B
import           Data.ByteString                                 (ByteString)
import           Data.Map.Strict                                        (empty,
                                                                  fromList)
import           Data.Typeable
import           Network.Transport                               (EndPointAddress (..), closeTransport)
import           Network.Transport.TCP                           (createTransport, defaultTCPParameters)

import           Check.System
import           Data.Dynamic
import           Data.Text                                       (Text)
import           Process.Agent
import           Process.Checker
import           Process.Watcher
import           System.Cron

host, port :: String
host = "localhost"
port = "10503"

remoteAddress :: ByteString
remoteAddress = "127.0.0.1:10501:0"

localhost = Hostname "localhost"

main :: IO ()
main = do
    Right t <- createTransport host port defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        cstart <-  mapM toChildStart [checker, agent localhost remoteAddress]
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

