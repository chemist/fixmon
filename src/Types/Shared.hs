{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Shared where

import           Types.Cron
import           Types.DslTypes

import           Control.Exception   (Exception)
import           Data.Map            (Map, empty)
import           Data.Monoid         (Monoid, mappend, mempty, (<>))
import           Data.Set            (Set)
import           Data.String         (IsString, fromString)
import           Data.Text           (Text, pack)
import           Data.Typeable       (Typeable)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Error (Error)
import           Data.Binary         (Binary, get, put)
import           Data.Text.Binary    ()

newtype HostId = HostId Int deriving (Show, Eq, Ord, Binary, Typeable, Read)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord, Binary, Typeable)


newtype GroupId = GroupId Int deriving (Show, Eq, Ord, Binary)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord, Binary)

instance IsString GroupName where
    fromString x = GroupName . pack $ x

data Group = Group
 { name     :: GroupName
 , hosts    :: Set HostId
 , triggers :: Set TriggerId
 , checks   :: Set CheckId
 } deriving Show


newtype CheckId = CheckId Int deriving (Show, Eq, Ord, Binary, Read)
newtype CheckHost = CheckHost (HostId, CheckId) deriving (Show, Eq, Ord, Binary, Typeable)
newtype CheckName = CheckName Text deriving (Eq, Ord, Binary)

instance Show CheckName where
    show (CheckName x) = show x

instance IsString CheckName where
    fromString x = CheckName . pack $ x

data Check = Check { _checkName :: CheckName
                   , _period    :: Cron
                   , _params    :: Map Text Text
                   } deriving (Show, Eq, Ord, Typeable)

instance Binary Check where
    put (Check a b c) = put a >> put b >> put c
    get = Check <$> get <*> get <*> get


newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord, Binary, Read)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord, Binary)

instance IsString TriggerName where
    fromString x = TriggerName . pack $ x

data Trigger = Trigger
  { _name        :: TriggerName
  , _description :: Text
  , _check       :: CheckId
  , _result      :: TriggerRaw Bool
  } deriving Show

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
 , _hosts     :: Map HostId Hostname
 , _groups    :: [Group]
 , _triggers  :: Map TriggerId Trigger
 , _checks    :: Map CheckId Check
 , _status    :: Map TriggerHost Status
 } deriving Show

data PError = PError String deriving (Show, Typeable)

instance Exception PError
instance Error PError


----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

emptyMonitoring :: Monitoring
emptyMonitoring = Monitoring empty empty empty mempty empty empty mempty


