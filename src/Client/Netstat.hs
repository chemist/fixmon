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
import qualified Data.Set as Set

netstat :: String
netstat = "proc"

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

data Point = Internet 
  { links :: Integer
  , port :: Port
  }        | Point
  { ip :: IP
  , port :: Port
  } deriving (Eq, Ord)


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


establishedIncoming :: Maybe Port -> [Connection] -> [Point]
establishedIncoming Nothing c = 
  let ports = Set.fromList . listeningPorts $ c 
      est = stateFilter TCP_ESTABLISHED c
  in map (\(Connection (Point _ p) (Point i _) _) -> Point i p) $ filter (\x -> Set.member (port . localSocket $ x) ports) est
establishedIncoming (Just p) c = 
  map (\(Connection (Point _ p') (Point i _) _) -> Point i p') $ filter (\x -> (port . localSocket $ x) == p) $ stateFilter TCP_ESTABLISHED c


establishedOutgoing :: Maybe Port -> [Connection] -> [Point]
establishedOutgoing Nothing c = 
  let ports = Set.fromList . listeningPorts $ c
      est = stateFilter TCP_ESTABLISHED c
  in remotePoint $ filter (\x -> not $ Set.member (port . localSocket $ x) ports) est
establishedOutgoing (Just p) c = 
  let ports = Set.fromList . listeningPorts $ c
      est = stateFilter TCP_ESTABLISHED c
  in remotePoint $ filter (\x -> (not $ Set.member (port . localSocket $ x) ports) && (port . remoteSocket $ x) == p) est



accumulatePoints :: [Point] -> [(Point, Integer)]
accumulatePoints p = Map.toList . Map.fromListWith (+) $ map (\x -> (x, 1)) p

accumulateIncoming :: Maybe Port -> [Connection] -> [(Point, Integer)]
accumulateIncoming p = accumulatePoints . establishedIncoming p

accumulateOutgoing :: Maybe Port -> [Connection] -> [(Point, Integer)]
accumulateOutgoing p = accumulatePoints . establishedOutgoing p

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

