{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Shared where

import           Control.DeepSeq
import           Types.Cron
import           Types.Dynamic 

import           Control.Monad       (mzero)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Set            (Set)
import           Data.String         (IsString, fromString)
import           Data.Text           (Text, pack)
import           Data.Yaml           (FromJSON (..), Value (..), parseJSON)

import           Control.Applicative (pure)
import           Data.Binary         (Binary)
import           Data.Text.Binary    ()
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Binary  ()
import           GHC.Generics        (Generic)
import Data.Typeable
import Network.Snmp.Client (Config, Version(..))
import Network.Protocol.Snmp (initial)

newtype HostId = HostId Int deriving (Show, Eq, Ord, Binary, Typeable, Read, NFData)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord, Binary, Typeable, NFData)

newtype GroupId = GroupId Int deriving (Show, Eq, Ord, Binary, NFData)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord, Binary, NFData)

instance IsString GroupName where
    fromString x = GroupName . pack $ x

data Group = Group
 { gname     :: !GroupName
 , ghosts    :: !(Set HostId)
 , gtriggers :: !(Set TriggerId)
 , gchecks   :: !(Set CheckId)
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

newtype CheckId = CheckId Int deriving (Show, Eq, Ord, Binary, Read, Typeable, NFData)
newtype CheckHost = CheckHost (HostId, CheckId, Maybe TriggerId) deriving (Show, Eq, Ord, Binary, Typeable, NFData)
newtype CheckName = CheckName Text deriving (Eq, Ord, Binary, Typeable)

instance Show CheckName where
    show (CheckName x) = show x

instance IsString CheckName where
    fromString x = CheckName . pack $ x

data Check = Check { cname   :: !CheckName
                   , chost   :: !Hostname
                   , cperiod :: !Cron
                   , ctype   :: !Text
                   , csnmp   :: Maybe Config
                   , cparams :: ![(Counter, Dyn)]
                   } deriving (Show, Typeable, Generic, Eq, Ord)

instance Ord Config where
    compare _ _ = EQ

newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord, Binary, Read, Typeable, NFData)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord, Binary, Typeable)

instance IsString TriggerName where
    fromString x = TriggerName . pack $ x

data Trigger = Trigger
  { tname        :: !TriggerName
  , tdescription :: !Text
  , tcheck       :: ![CheckId]
  , tresult      :: !ETrigger
  } deriving (Show, Eq, Typeable, Generic)

instance Ord Trigger where
    compare x y = compare (tname x) (tname y)

newtype Status = Status { unStatus :: Bool } deriving (Show, Eq, Ord, Binary)

newtype TriggerFun = TriggerFun (Complex -> Status)

instance Show TriggerFun where
     show _ = "trigger fun here"

instance FromJSON Hostname where
    parseJSON (String x) = pure $ Hostname x
    parseJSON _ = mzero

----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

