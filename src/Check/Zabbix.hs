{-# LANGUAGE OverloadedStrings #-}
module Check.Zabbix where

import Check.Zabbix.Types

import Data.Conduit.Network
import Data.ByteString (ByteString)
import Data.Conduit.Binary (sinkHandle, sourceLbs)
import Data.Conduit
import System.IO (stdout)
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)


-- <HEADER> - "ZBXD\x01" (5 байт)
-- <DATALEN> - длина данных (8 байт). 1 будет сформировано как 01/00/00/00/00/00/00/00 (восемь байт в HEX, то есть число в 64-битном формате)
--


agentHost :: ByteString
agentHost = "limbo"

agentPort :: Int
agentPort = 10050

main :: IO ()
main = runTCPClient (clientSettings agentPort agentHost) $ \app -> do
    void . forkIO $ getter $$ appSink app
    appSource app $$ sinkHandle stdout


getter :: MonadIO m => Source m ByteString
getter = sourceLbs $ raw AgentPing

