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

host, port :: String
host = "localhost"
port = "10503"

remoteAddress :: ByteString
remoteAddress = "127.0.0.1:10501:0"

main :: IO ()
main = do
    Right t <- createTransport host port defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ forever $ do
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



testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", "http://www.ubank.neta") ])
                                                                      --   , ("redirects", "3") ])

