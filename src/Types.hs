{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types
( HostId
, Hostname(..)
-- * Триггеры
, TriggerId
, TriggerName(..)
, Trigger(..)
, TriggerHost(..)
, TriggerHostChecks(..)
-- * Проверки
, Check(..)
, CheckId
, CheckName(..)
, CheckHost(..)
, Route
, Checkable(..)
, RouteCheck
, ToComplex(..)
-- * Группы
, GroupName(..)
, GroupId
, Group(..)
-- * Общее
, Cron(..)
, Status(..)
, Monitoring(..)
-- , Log(..)
, routeCheck'
, Database(..)
, DBException(..)
-- * monads
-- * error
-- * for tests
-- , testHttp
, module Types.Dynamic
) where

import           Data.Map.Strict     (Map)
import           Data.Set            (Set)
import           Data.Vector         (Vector)
import           Network.Snmp.Client (Config)
import           Storage.InfluxDB    (InfluxDB)
import qualified Storage.InfluxDB    as InfluxDB
import           Types.Check
import           Types.Cron
import           Types.Dynamic
import           Types.Shared
import           Check.Snmp.Snmp (Rules)

class Database db where
    getData :: db -> Table -> Fun -> IO Dyn
    saveData :: db -> [Complex] -> IO ()
    config :: db

data Monitoring = Monitoring
 { periodMap  :: !(Map Cron (Set CheckHost))
 , hosts      :: !(Vector Hostname)
 , groups     :: !(Vector Group)
 , triggers   :: !(Vector Trigger)
 , checks     :: !(Vector Check)
-- , status    :: !(Map TriggerHost Status)
 , triggerMap:: !(Map CheckHost (Set TriggerId))
 , snmp       :: !Config
 , snmpRules  :: !Rules
 , storage    :: !InfluxDB
 } deriving Show

instance Database InfluxDB where
    getData = InfluxDB.getData
    saveData = InfluxDB.saveData
    config = InfluxDB.config



