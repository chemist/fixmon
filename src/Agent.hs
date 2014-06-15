{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Main where

import           Check
import           Check.Http
import           Check.System
import           Types                                      (Check (..),
                                                             CheckName (..),
                                                             Cron (..))

import           Control.Concurrent                         (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types hiding (agent)
import           Control.Distributed.Process.Node
import           Control.Monad
import qualified Control.Monad.State                        as ST
import           Control.Monad.Trans.Class
import           Data.Binary
import qualified Data.Binary                                as B
import           Data.ByteString                            (ByteString)
import           Data.ByteString.Lazy                       (toStrict)
import           Data.Map                                   (fromList)
import           Data.Map                                   (empty)
import           Data.Maybe                                 (fromMaybe)
import           Data.Typeable
import           Network.Transport                          (EndPointAddress (..),
                                                             closeTransport)
import           Network.Transport.TCP                      (createTransport, defaultTCPParameters)
import           Process.Utils
import           System.Cron

data H = forall a. Checkable a => H {unH :: a}

checks :: [H]
checks = [H HttpSimple, H Shell]

host = "localhost"
port = "10503"
remoteAddress = "127.0.0.1:10501:0"

main :: IO ()
main = do
    Right t <- createTransport host port defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ forever $ do
        whereisRemoteAsync (NodeId (EndPointAddress remoteAddress)) "registrator"
        mregistrator <- expectTimeout 1000000 :: Process (Maybe WhereIsReply)
        maybe (say "registrator not found") good (mmm mregistrator)
        liftIO $ threadDelay 1000000
    -- _ <- liftIO getLine :: Process String
    closeLocalNode node
    closeTransport t
        where
          mmm :: Maybe WhereIsReply -> Maybe ProcessId
          mmm (Just (WhereIsReply _ x)) = x
          mmm _ = Nothing
          remot = NodeId (EndPointAddress remoteAddress)
          good pid = do
              say "registrator found"
              send pid =<< getSelfPid
              r <- expect :: Process Reg
              say $ "success registered"
              runCheckT (agent pid) empty
              return ()

type Agent = CheckT Process

agent :: ProcessId -> Agent ()
agent pid = do
    mapM_ (\(H x) -> addRoute x) checks
    st <- ST.get
    forever $ lift . receiveWait $ map match [ \x -> runCheckT (receiveCheck pid x) st ]

receiveCheck :: ProcessId -> Check -> Agent ()
receiveCheck pid ch = do
    lift $ say $ show ch
    r <- runCheck ch
    lift $ say $ show r


data Reg = Reg deriving (Show, Typeable)

instance B.Binary Reg where
    put _ = B.put (0::Int)
    get = do
        x <- B.get :: B.Get Int
        return Reg



testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", "http://www.ubank.neta") ])
                                                                      --   , ("redirects", "3") ])

