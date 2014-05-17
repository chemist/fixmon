{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
module Process.Utils where


import           Control.Applicative
import           Control.Distributed.Process (Process, ProcessId, expect,
                                              getSelfPid, nsend)
import           Data.Binary
import           Data.Typeable

newtype Request a = Request (ProcessId, a) deriving (Eq, Ord, Show, Typeable, Binary)
newtype Response a =  Response (Maybe a) deriving (Eq, Ord, Show, Typeable, Binary)

request :: forall a t. (Typeable a, Typeable t, Binary a, Binary t) => a -> Process (Maybe t)
request i = do
    self <- getSelfPid
    nsend "configurator" (Request (self, i))
    Response r <- expect
    return r

data UtilMessage = MinuteMessage
                 | CronMap
                 | HostMap
                 | CheckMap
                 deriving (Typeable, Show, Eq, Enum)

instance Binary UtilMessage where
    put MinuteMessage = put (0 :: Word8)
    put CronMap       = put (1 :: Word8)
    put HostMap       = put (2 :: Word8)
    put CheckMap      = put (3 :: Word8)
    get = do
        a <- get :: Get Word8
        case a of
             0 -> pure MinuteMessage
             1 -> pure CronMap
             2 -> pure HostMap
             3 -> pure CheckMap
             _ -> error "bad binary"


