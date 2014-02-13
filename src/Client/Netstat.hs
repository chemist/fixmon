{-# LANGUAGE OverloadedStrings #-}
module Netstat where

import Data.Text.IO
import Data.Text hiding (reverse, filter, map)
import qualified Data.Text.Read as RT
import Data.Attoparsec.Text 
import qualified Data.Attoparsec.Text as AT
import Data.Network.Ip
import Data.Word
import Control.Applicative
import Prelude hiding (readFile, concat)
import qualified Data.Map as Map

netstat :: String
netstat = "proc"

localIp :: [IP]
localIp = ["127.0.0.1"]

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

data Point = Point
  { ip :: IP
  , port :: Port
  } deriving (Eq)

instance Show Point where
    show x = show (ip x) ++ ":" ++ show (port x)

data Connection = Connection 
  { localSocket :: From
  , remoteSocket :: To 
  , stateSocket :: TcpState
  } deriving (Show, Eq)

stateFilter :: TcpState -> [Connection] -> [Connection]
stateFilter st = filter (\(Connection _ _ s) -> s == st) 

localPoint :: [Connection] -> [Point]
localPoint = map (\x -> localSocket x)

remotePoint :: [Connection] -> [Point]
remotePoint = map (\x -> remoteSocket x)

listening :: [Connection] -> [Point]
listening = localPoint . stateFilter TCP_LISTEN 

listeningPorts :: [Connection] -> [Port]
listeningPorts = map port . listening

establishedFrom :: Maybe IP -> Maybe Port -> [Connection] -> [Point]
establishedFrom Nothing Nothing = remotePoint . stateFilter TCP_ESTABLISHED
establishedFrom Nothing (Just p) = remotePoint . filter (\x -> (port . localSocket $ x) == p) . stateFilter TCP_ESTABLISHED
establishedFrom (Just i) Nothing = remotePoint . filter (\x -> (ip . localSocket $ x) == i) . stateFilter TCP_ESTABLISHED
establishedFrom (Just i) (Just p) = remotePoint . filter (\x -> Point i p == localSocket x) . stateFilter TCP_ESTABLISHED

accumulatePoints :: [Point] -> [(IP, Integer)]
accumulatePoints p = Map.toList . Map.fromListWith (+) $ map (\x -> (ip x, 1)) p

accumulatePointsByPort :: Port -> [Connection] -> [(IP, Integer)]
accumulatePointsByPort p = accumulatePoints . establishedFrom Nothing (Just p) 

accumulateClientsByPort :: [Connection] -> [(Port, Integer)]
accumulateClientsByPort c = undefined

accumulateServersByPort :: [Connection] -> [(Point, Integer)]
accumulateServersByPort = undefined

firstLine :: Parser ()
firstLine = skipWhile (not . isEndOfLine)

connection :: Parser Connection
connection = do
    _ <- many space <* number <* char ':' <* space
    f <- leHexToIp <$> (takeTill (== ':') <* char ':')
    p <- AT.hexadecimal <* space
    t <- leHexToIp <$> (takeTill (== ':') <* char ':')
    p' <- AT.hexadecimal <* space
    status <- AT.hexadecimal <* skipWhile (not . isEndOfLine)
    return $ Connection (Point f p) (Point t p') (toEnum $ status - 1 )

connections :: Parser [Connection]
connections = firstLine *> many connection 

