{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Process.Configurator.Message where


import           Control.Distributed.Process (ProcessId, Process, getSelfPid, nsend, expect)
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

