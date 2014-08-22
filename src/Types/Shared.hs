{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Shared where

import           Types.Cron
import           Types.DslTypes

import           Control.Monad       (mzero)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid         (Monoid, mappend, mempty, (<>))
import           Data.Set            (Set)
import           Data.String         (IsString, fromString)
import           Data.Text           (Text, pack)
import           Data.Yaml           (FromJSON (..), Value (..), parseJSON)

import           Control.Applicative (pure, (<$>))
import           Data.Binary         (Binary, Get, get, getWord8, put, putWord8)
import           Data.Dynamic
import           Data.Text.Binary    ()
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Binary  ()
import           GHC.Generics        (Generic)

newtype HostId = HostId Int deriving (Show, Eq, Ord, Binary, Typeable, Read)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord, Binary, Typeable)

newtype GroupId = GroupId Int deriving (Show, Eq, Ord, Binary)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord, Binary)

instance IsString GroupName where
    fromString x = GroupName . pack $ x

data Group = Group
 { gname     :: !GroupName
 , ghosts    :: Set HostId
 , gtriggers :: Set TriggerId
 , gchecks   :: Set CheckId
 } deriving (Show, Generic)

instance Binary Group

class IntId a where
    unId :: a -> Int
    pId :: Int -> a

instance IntId CheckId where
    unId (CheckId x) = x
    pId = CheckId

instance IntId HostId where
    unId (HostId x) = x
    pId = HostId

instance IntId TriggerId where
    unId (TriggerId x) = x
    pId = TriggerId

instance IntId GroupId where
    unId (GroupId x) = x
    pId = GroupId

newtype CheckId = CheckId Int deriving (Show, Eq, Ord, Binary, Read, Typeable)
newtype CheckHost = CheckHost (HostId, CheckId, Maybe TriggerId) deriving (Show, Eq, Ord, Binary, Typeable)
newtype CheckName = CheckName Text deriving (Eq, Ord, Binary, Typeable)

instance Show CheckName where
    show (CheckName x) = show x

instance IsString CheckName where
    fromString x = CheckName . pack $ x

data Check = Check { cname   :: CheckName
                   , cperiod :: Cron
                   , ctype   :: Text
                   , cparams :: Map Text Dynamic
                   } deriving (Show, Typeable, Generic, Eq, Ord)

instance Eq Dynamic where
    a == b = dynTypeRep a == dynTypeRep b

instance Ord Dynamic where
    compare a b = compare (dynTypeRep a) (dynTypeRep b)

textType, intType, boolType, doubleType :: TypeRep
textType = typeOf (undefined :: Text)
intType = typeOf (undefined :: Int)
boolType = typeOf (undefined :: Bool)
doubleType = typeOf (undefined :: Double)

instance Binary Dynamic where
    put x
      | dynTypeRep x == textType = putWord8 0 >> put (fromDyn x (undefined::Text))
      | dynTypeRep x == intType = putWord8 1 >> put (fromDyn x (undefined::Int))
      | dynTypeRep x == boolType = putWord8 2 >> put (fromDyn x (undefined::Bool))
      | dynTypeRep x == doubleType = putWord8 3 >> put (fromDyn x (undefined::Double))
      | otherwise = undefined
    get = unpackDyn =<< getWord8
      where
      unpackDyn 0 = toDyn <$> (get :: Get Text)
      unpackDyn 1 = toDyn <$> (get :: Get Int)
      unpackDyn 2 = toDyn <$> (get :: Get Bool)
      unpackDyn 3 = toDyn <$> (get :: Get Double)
      unpackDyn _ = undefined


instance Binary Check
newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord, Binary, Read, Typeable)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord, Binary, Typeable)

instance IsString TriggerName where
    fromString x = TriggerName . pack $ x

data Trigger = Trigger
  { tname        :: !TriggerName
  , tdescription :: !Text
  , tcheck       :: [CheckId]
  , tresult      :: TriggerRaw Bool
  } deriving (Show, Eq, Typeable, Generic)

instance Ord Trigger where
    compare x y = compare (tname x) (tname y)

instance Binary Trigger

newtype Log = Log Text deriving (Show, Eq, Typeable)

instance IsString Log where
    fromString x = Log . pack $ x

instance Monoid Log where
    mempty = Log ""
    Log x `mappend` Log y = Log $ x <> y

newtype Status = Status { unStatus :: Bool } deriving (Show, Eq, Ord, Binary)

newtype Complex = Complex (Map Text Any) deriving (Show, Eq, Binary, Typeable)

newtype TriggerFun = TriggerFun (Complex -> Status)

instance Show TriggerFun where
     show _ = "trigger fun here"

data StartOptions = StartOptions
  { config :: FilePath
  } deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map Cron (Set CheckHost)
 , _hosts     :: Vector Hostname
 , _groups    :: Vector Group
 , _triggers  :: Vector Trigger
 , _checks    :: Vector Check
 , _status    :: Map TriggerHost Status
 } deriving Show

instance FromJSON Hostname where
    parseJSON (String x) = pure $ Hostname x
    parseJSON _ = mzero

----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

emptyMonitoring :: Monitoring
emptyMonitoring = Monitoring M.empty V.empty V.empty V.empty V.empty M.empty

