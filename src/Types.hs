{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
, CheckException(..)
-- * Группы
, GroupName(..)
, GroupId
, Group(..)
-- * Общее
, Cron(..)
, Status(..)
, Monitoring(..)
-- , Log(..)
, IntId(..)
, routeCheck'
, Database(..)
, DBException(..)
-- * monads
-- * error
-- * for tests
-- , testHttp
, module Types.Dynamic
) where

import           Types.Check
import           Types.Cron
import           Types.Dynamic
import           Types.Shared
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import Network.Snmp.Client (Config)
import Storage.InfluxDB (InfluxDB)
import qualified Storage.InfluxDB as InfluxDB
-- import           Network.HTTP.Conduit (Request)
-- import qualified Network.HTTP.Types.Status as H
-- import Data.Typeable
--

class Database db where
    getData :: db -> Table -> Fun -> IO Dyn
    saveData :: db -> [(Hostname, [Complex])] -> IO ()
    config :: db

data Monitoring = Monitoring
 { periodMap :: !(Map Cron (Set CheckHost))
 , hosts     :: !(Vector Hostname)
 , groups    :: !(Vector Group)
 , triggers  :: !(Vector Trigger)
 , checks    :: !(Vector Check)
 , status    :: !(Map TriggerHost Status)
 , thc       :: !(Set TriggerHostChecks)
 , snmp      :: !Config
 , storage   :: !InfluxDB
 } deriving Show


-- emptyMonitoring :: Monitoring
-- emptyMonitoring = Monitoring M.empty V.empty V.empty V.empty V.empty M.empty (initial Version3) (config :: InfluxDB)

instance Database InfluxDB where
    getData = InfluxDB.getData
    saveData = InfluxDB.saveData
    config = InfluxDB.config



