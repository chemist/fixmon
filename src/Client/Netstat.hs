{-# LANGUAGE OverloadedStrings #-}
module Netstat where

import Data.Text.IO
import Data.Text hiding (reverse)
import qualified Data.Text.Read as RT
import Data.Attoparsec.Text 
import qualified Data.Attoparsec.Text as AT
import Data.Network.Ip
import Data.Word
import Control.Applicative
import Prelude hiding (readFile, concat)

netstat :: String
netstat = "proc"

hexIp :: Text
hexIp = "0100007F"

leHexToIp :: Text -> IP
leHexToIp h = let Right (i, _) = RT.hexadecimal . invert $ h
                  conv = toEnum . fromEnum :: Int -> Word32
                  invert = concat . reverse . chunksOf 2 
              in IPv4 $ conv i

main' :: IO (Either String [Connection] )
main' = do
    file <- readFile netstat
    return $ parseOnly connections file

{--
   enum {
        TCP_ESTABLISHED = 1,
        TCP_SYN_SENT,
        TCP_SYN_RECV,
        TCP_FIN_WAIT1,
        TCP_FIN_WAIT2,
        TCP_TIME_WAIT,
        TCP_CLOSE,
        TCP_CLOSE_WAIT,
        TCP_LAST_ACK,
        TCP_LISTEN,
        TCP_CLOSING,    /* Now a valid state */

    TCP_MAX_STATES  /* Leave at the end! */
        };
        --}
    
data TcpState = TCP_ESTABLISHED
              | TCP_SYN_SENT
              | TCP_SYN_RECV
              | TCP_FIN_WAIT1
              | TCP_FIN_WAIT2
              | TCP_TIME_WAIT
              | TCP_CLOSE
              | TCP_CLOSE_WAIT
              | TCP_LAST_ACK
              | TCP_LISTEN
              | TCP_CLOSING   
              | TCP_MAX_STATES deriving (Show, Enum, Eq, Ord) 

str :: Text
str = "   0: 00000000:2387 00000000:0000 0A 00000000:00000000 00:00000000 00000000   114        0 13687 1 0000000000000000 100 0 0 10 -1\n"

type Port = Int
type From = Point
type To = Point

data Point = Point IP Port deriving (Show, Eq)
data Connection = Connection From To deriving (Show, Eq)


firstLine :: Parser ()
firstLine = skipWhile (not . isEndOfLine)

connection :: Parser Connection
connection = do
    _ <- many space <* number <* char ':' <* space
    f <- leHexToIp <$> (takeTill (== ':') <* char ':')
    p <- AT.hexadecimal <* space
    t <- leHexToIp <$> (takeTill (== ':') <* char ':')
    p' <- AT.hexadecimal <* space <* skipWhile (not . isEndOfLine)
    return $ Connection (Point f p) (Point t p')

connections :: Parser [Connection]
connections = firstLine *> many connection 

