{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main where

import           Check.Http
import           Check.System                     ()
import           Types                            (Check (..), CheckName (..), Hostname (..),
                                                   Cron (..), Checkable(..))

import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Platform.Supervisor
import           Control.Distributed.Process.Platform
import           Control.Monad
import qualified Control.Monad.State              as ST
import           Control.Monad.Trans.Class
import           Data.Binary
import qualified Data.Binary                      as B
import           Data.ByteString                  (ByteString)
import           Data.Map                         (empty, fromList)
import           Data.Typeable
import           Network.Transport                (EndPointAddress (..),
                                                   closeTransport)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)

import           Process.Watcher
import           System.Cron
import           Process.RegisterAgent

host, port :: String
host = "localhost"
port = "10503"

remoteAddress :: ByteString
remoteAddress = "127.0.0.1:10501:0"

main :: IO ()
main = do
    Right t <- createTransport host port defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        cstart <-  mapM toChildStart [registerAgent] 
        let cspec = map child $ zip cstart ["registerAgent"]
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

testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", "http://www.ubank.neta") ])
                                                                      --   , ("redirects", "3") ])

