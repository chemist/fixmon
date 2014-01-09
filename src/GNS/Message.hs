{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GNS.Message where
import           Control.Applicative
import           Control.Distributed.Process (ProcessId)
import           Data.Binary
import           Data.Map                    (Map)
import           Data.Set                    (Set)
import           Data.Typeable
import           GNS.Data
import           System.Cron

data CMes = MinuteMessage
          | Reload (Map Cron (Set CheckHost)) deriving Typeable

newtype Task = Task CheckHost deriving Typeable

data SMes a = GetHosts ProcessId
            | GetHostsId ProcessId
            | GetHost ProcessId HostId
            | GetTrigger ProcessId TriggerId
            | GetCheck ProcessId CheckId
            | GetCronMap ProcessId
            | SMes ProcessId a deriving (Typeable, Show, Eq)

instance Binary a => Binary (SMes a) where
    put (GetHosts x)     = put (0 :: Word8) >> put x
    put (GetHostsId x)   = put (1 :: Word8) >> put x
    put (GetHost x y )   = put (2 :: Word8) >> put x >> put y
    put (GetTrigger x y) = put (3 :: Word8) >> put x >> put y
    put (GetCheck x y)   = put (4 :: Word8) >> put x >> put y
    put (GetCronMap x)   = put (5 :: Word8) >> put x
    put (SMes x a)       = put (6 :: Word8) >> put x >> put a
    get = do
        a <- get :: Get Word8
        case a of
             0 -> GetHosts   <$> get
             1 -> GetHostsId <$> get
             2 -> GetHost    <$> get <*> get
             3 -> GetTrigger <$> get <*> get
             4 -> GetCheck   <$> get <*> get
             5 -> GetCronMap <$> get
             6 -> SMes       <$> get <*> get
             _ -> error "bad binary Mesg"

instance Binary CMes where
    put MinuteMessage = put (0 :: Word8)
    put (Reload x) = put (1 :: Word8) >> put x
    get = do
        a <- get :: Get Word8
        case a of
             0 -> pure MinuteMessage
             1 -> Reload <$> get
             _ -> error "bad binary"

instance Binary Task where
    put (Task (CheckHost (HostId x, CheckId y))) = put x >> put y
    get = do
        x <- get
        y <- get
        return $ Task (CheckHost (HostId x, CheckId y))

