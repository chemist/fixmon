{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Shared where

import           Types.Cron
import           Types.DslTypes

import           Control.Exception   (Exception)
import           Control.Monad       (mzero)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid         (Monoid, mappend, mempty, (<>))
import           Data.Set            (Set)
import           Data.String         (IsString, fromString)
import           Data.Text           (Text, pack)
import           Data.Typeable       (Typeable)
import           Data.Yaml           (FromJSON (..), Value (..), parseJSON)

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad.Error (Error)
import           Data.Binary         (Binary, get, put)
import           Data.Text.Binary    ()
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Binary  ()

newtype HostId = HostId Int deriving (Show, Eq, Ord, Binary, Typeable, Read)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord, Binary, Typeable)

newtype GroupId = GroupId Int deriving (Show, Eq, Ord, Binary)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord, Binary)

instance IsString GroupName where
    fromString x = GroupName . pack $ x

data Group = Group
 { gname     :: GroupName
 , ghosts    :: Set HostId
 , gtriggers :: Set TriggerId
 , gchecks   :: Set CheckId
 } deriving Show

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
newtype CheckHost = CheckHost (HostId, CheckId) deriving (Show, Eq, Ord, Binary, Typeable)
newtype CheckName = CheckName Text deriving (Eq, Ord, Binary, Typeable)

instance Show CheckName where
    show (CheckName x) = show x

instance IsString CheckName where
    fromString x = CheckName . pack $ x

data Check = Check { cname   :: CheckName
                   , cperiod :: Cron
                   , ctype   :: Text
                   , cparams :: Map Text Text
                   } deriving (Show, Eq, Ord, Typeable)

instance Binary Check where
    put (Check a b c d) = put a >> put b >> put c >> put d
    get = Check <$> get <*> get <*> get <*> get


newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord, Binary, Read, Typeable)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord, Binary, Typeable)

instance IsString TriggerName where
    fromString x = TriggerName . pack $ x

data Trigger = Trigger
  { tname        :: TriggerName
  , tdescription :: Text
  , tcheck       :: CheckId
  , tresult      :: TriggerRaw Bool
  } deriving (Show, Eq, Typeable)

instance Binary Trigger where
    put (Trigger n d c r) = put n >> put d >> put c >> put r
    get = Trigger <$> get <*> get <*> get <*> get


newtype Log = Log Text deriving (Show, Eq, Typeable)

instance IsString Log where
    fromString x = Log . pack $ x

instance Monoid Log where
    mempty = Log ""
    Log x `mappend` Log y = Log $ x <> y

newtype Status = Status { unStatus :: Bool } deriving (Show, Eq, Ord, Binary)

newtype Complex = Complex (Map Text Any) deriving Show

newtype TriggerFun = TriggerFun (Complex -> Status)

instance Show TriggerFun where
     show _ = "trigger fun here"

data StartOptions = StartOptions
  { config :: FilePath
  } deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map Cron (Set CheckHost)
 , _checkHost :: Map CheckHost (Set TriggerId)
 , _hosts     :: Vector Hostname
 , _groups    :: Vector Group
 , _triggers  :: Vector Trigger
 , _checks    :: Vector Check
 , _status    :: Map TriggerHost Status
 } deriving Show

data PError = PError String deriving (Show, Typeable)

instance Exception PError
instance Error PError

instance FromJSON Hostname where
    parseJSON (String x) = pure $ Hostname x
    parseJSON _ = mzero

----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

emptyMonitoring :: Monitoring
emptyMonitoring = Monitoring M.empty M.empty V.empty V.empty V.empty V.empty M.empty

