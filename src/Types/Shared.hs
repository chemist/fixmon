{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Shared where

import           Control.DeepSeq
import           Types.Cron
import           Types.Dynamic

import           Control.Monad       (mzero)
import           Data.Set            (Set)
import           Data.String         (IsString, fromString)
import           Data.Text           (Text, pack)
import           Data.Yaml           (FromJSON (..), Value (..), parseJSON)

import           Data.Binary         (Binary)
import           Data.Scientific     (floatingOrInteger)
import           Data.Text.Binary    ()
import           Data.Typeable
import           GHC.Generics        (Generic)
import           Network.Snmp.Client (Config)

newtype HostId = HostId Int deriving (Show, Eq, Ord, Binary, Typeable, Read, NFData)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord, Binary, Typeable, NFData)

newtype GroupId = GroupId Int deriving (Show, Eq, Ord, Binary, NFData)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord, Binary, NFData)

instance IsString GroupName where
    fromString x = GroupName . pack $ x

instance Convert HostId Dyn where
    to (HostId x) = Number . fromIntegral $ x
    from (Number x) = case (floatingOrInteger x :: Either Double Int) of
                           Right d -> HostId d
                           Left _ -> error "hostid must be integer"
    from _ = error "convert hostid, only number here"

instance Convert CheckId Dyn where
    to (CheckId x) = Number . fromIntegral $ x
    from (Number x) = case (floatingOrInteger x :: Either Double Int) of
                           Right d -> CheckId d
                           Left _ -> error "checkid must be integer"
    from _ = error "convert hostid, only number here"

instance Convert Hostname Dyn where
    to (Hostname x) = String x
    from (String x) = Hostname x
    from _ = error "convert hostid, only number here"

data Group = Group
 { gname     :: !GroupName
 , ghosts    :: !(Set HostId)
 , gtriggers :: !(Set TriggerId)
 , gchecks   :: !(Set CheckId)
 } deriving (Show, Generic)

instance Binary Group

instance Convert Int CheckId where
    from (CheckId x) = x
    to = CheckId

instance Convert Int HostId where
    from (HostId x) = x
    to = HostId

instance Convert Int TriggerId where
    from (TriggerId x) = x
    to = TriggerId

instance Convert Int GroupId where
    from (GroupId x) = x
    to = GroupId

instance Convert Text Hostname where
    from (Hostname x) = x
    to = Hostname

newtype CheckId = CheckId Int deriving (Show, Eq, Ord, Binary, Read, Typeable, NFData)
newtype CheckHost = CheckHost (HostId, CheckId) deriving (Show, Binary, Typeable, NFData, Eq, Ord)

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
                   , cparams :: !Value
                   } deriving (Show, Typeable, Generic, Eq, Ord)

instance Ord Config where
    compare _ _ = EQ

newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord, Binary, Read, Typeable, NFData)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)
newtype TriggerHostChecks = TriggerHostChecks (HostId, TriggerId, [CheckId]) deriving (Show, Eq, Ord)
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord, Binary, Typeable)

instance IsString TriggerName where
    fromString x = TriggerName . pack $ x

data Trigger = Trigger
  { tname        :: !TriggerName
  , tdescription :: !Text
  , tcheck       :: ![CheckId]
  , tresult      :: !Exp
  } deriving (Show, Eq, Typeable, Generic)

instance Ord Trigger where
    compare x y = compare (tname x) (tname y)

-- newtype Status = Status Bool deriving (Show, Eq, Ord, Binary)

data Status = Ok
            | SomethingWrong Text
            | Bad Text
            deriving (Show, Eq, Ord)

newtype TriggerFun = TriggerFun (Complex -> Status)

instance Show TriggerFun where
     show _ = "trigger fun here"

instance FromJSON Hostname where
    parseJSON (String x) = pure $ Hostname x
    parseJSON _ = mzero

----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

