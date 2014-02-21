{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Netstat where

import Data.Text.IO
import Data.Text hiding (reverse, filter, map)
import qualified Data.Text.Read as RT
import Data.Attoparsec.Text hiding (I)
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

data Point = Point
  { ip :: IP
  , port :: Port
  , name :: Maybe Text
  } deriving (Eq, Ord)

instance Show Point where
    show (Point i p (Just n)) = unpack n ++ " " ++ show i ++ " " ++ show p
    show (Point i p Nothing) = "Internet " ++ show i ++ " " ++ show p

newtype Dns = Dns [(Net, Text)]

data Net = II IP
         | NN IPSubnet deriving (Show)

instance Eq Net where
    (II x) == (II y) =  x == y
    NN x == NN y = x == y
    II x == NN y = isHostInNetwork x y
    NN y == II x = isHostInNetwork x y

knownIp :: Dns
knownIp = Dns  [ (II $ read "127.0.0.1", "localhost")
               , (II $ read "10.12.0.54", "gray local")
               , (II $ read "188.138.95.252", "public web02.intaxi.ru")
               , (II $ read "91.224.183.150", "office")
               , (II $ read "217.28.217.83", "geo.intaxi.ru")
               , (II $ read "50.31.164.146", "newrelic")
               , (NN $ read "172.18.60.0/24", "vpn network")
               , (II $ read "172.18.60.51", "idb00.intaxi")
               , (NN $ read "127.0.0.0/8", "vpn network")
               ]

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
  in map (\(Connection (Point _ p _) (Point i _ n) _) -> Point i p n) $ filter (\x -> Set.member (port . localSocket $ x) ports) est
establishedIncoming (Just p) c = 
  map (\(Connection (Point _ p' _) (Point i _ n) _) -> Point i p' n) $ filter (\x -> (port . localSocket $ x) == p) $ stateFilter TCP_ESTABLISHED c


establishedOutgoing :: Maybe Port -> [Connection] -> [Point]
establishedOutgoing Nothing c = 
  let ports = Set.fromList . listeningPorts $ c
      est = stateFilter TCP_ESTABLISHED c
  in remotePoint $ filter (\x -> not $ Set.member (port . localSocket $ x) ports) est
establishedOutgoing (Just p) c = 
  let ports = Set.fromList . listeningPorts $ c
      est = stateFilter TCP_ESTABLISHED c
  in remotePoint $ filter (\x -> (not $ Set.member (port . localSocket $ x) ports) && (port . remoteSocket $ x) == p) est

toN :: Dns -> [Point] -> [Point]
toN (Dns a) l = map (fun a) l
                where
                fun y (Point i p _) = case lookup (II i) y of
                                           Nothing -> Point i p Nothing
                                           Just n -> Point i p (Just n)

newtype Incoming = Incoming (Map.Map Port (Set.Set Point)) deriving (Show)

newtype Outgoing = Outgoing (Map.Map Port (Set.Set Point)) deriving (Show)

allEstablished :: Dns -> [Connection] -> (Incoming, Outgoing)
allEstablished d l = 
  let estI = map (\x -> (port x, Set.singleton x)) $ toN d $ establishedIncoming Nothing l
      estO = map (\x -> (port x, Set.singleton x)) $ toN d $ establishedOutgoing Nothing l
  in (Incoming $ Map.fromListWith Set.union estI, Outgoing $ Map.fromListWith Set.union estO)


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
    return $ Connection (Point f p Nothing) (Point t p' Nothing) (toEnum $ status - 1 )

connections :: Parser [Connection]
connections = firstLine *> many connection 

