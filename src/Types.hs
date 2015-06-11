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
-- * Группы
, GroupName(..)
, GroupId
, Group(..)
-- * Общее
, Cron(..)
, Status(..)
, Monitoring(task, snmpRules, storage)
, mkMonitoring
, Task(..)
, Prefix
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
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Vector         (Vector)
import qualified Data.Vector as V
import           Storage.InfluxDB    (InfluxDB)
import qualified Storage.InfluxDB    as InfluxDB
import           Types.Check
import           Types.Cron
import           System.Cron
import           Types.Dynamic
import           Types.Shared
import           Check.Snmp.Snmp (Rules)
import Data.Text (Text)
import           Data.Time.Clock
import Data.Monoid ((<>))

class Database db where
    getData :: db -> Text -> Fun -> IO Dyn
    saveData :: db -> [Complex] -> IO ()
    config :: db

data Monitoring = Monitoring
 { _hosts      :: !(Vector Hostname)
 , _checks     :: !(Vector Check)
 , _triggers   :: !(Vector Trigger)
 , _periodMap  :: !(Map Cron (Set CheckHost))
 , _triggerMap :: !(Map CheckHost (Set TriggerId))
 , _groups     :: !(Vector Group)
 , snmpRules  :: !Rules
 , storage    :: !InfluxDB
 , task       :: UTCTime -> Tasks
 } 

mkMonitoring :: Vector Hostname
              -> Vector Check
              -> Vector Trigger
              -> Vector Group
              -> Rules
              -> InfluxDB
              -> Monitoring
mkMonitoring a b c f h m =
    let periodMap = cronChecks b c f
        triggerMap = triggersMap f c
        tasks = taskGenerator a b c periodMap triggerMap
    in Monitoring a b c periodMap triggerMap f h m tasks

data Task = Task Hostname Check (Set Trigger) deriving (Eq, Ord)

instance Show Task where
    show (Task h c st) =
        "Task:---------------------------\n\thost: " <> show h <>
            "\n\tcheck: " <> show c <>
                "\n\ttriggers: " <> show st <> "\n\n"

type Tasks = Set Task

taskGenerator :: Vector Hostname
              -> Vector Check
              -> Vector Trigger
              -> PeriodMap
              -> Map CheckHost (Set TriggerId)
              -> UTCTime -> Tasks
taskGenerator vHostname vChecks vTrigger vPeriodMap vTriggerMap now =
    let vCheckHosts = Set.unions . Map.elems $ Map.filterWithKey (\(Cron x) _ -> scheduleMatches x now) vPeriodMap
        triggersByCheckHost ch = Set.map triggerByTriggerId $ maybe Set.empty id $ Map.lookup ch vTriggerMap
        triggerByTriggerId (TriggerId t) = vTrigger V.! t
        hostByCheckHost (CheckHost (HostId h, _)) = vHostname V.! h
        checkByCheckHost (CheckHost (_, CheckId c)) = vChecks V.! c
    in Set.map (\x -> Task (hostByCheckHost x) ((checkByCheckHost x) { chost = (hostByCheckHost x)} ) (triggersByCheckHost x)) vCheckHosts

--------------------------------------------------------------------------------------------------
    -- _periodMap
--------------------------------------------------------------------------------------------------

type PeriodMap = Map Cron (Set CheckHost)

cronChecks :: Vector Check -> Vector Trigger -> Vector Group -> PeriodMap
cronChecks vc vt vg = Map.fromSet fun cronSet
  where 
    fun :: Cron -> Set.Set CheckHost
    fun c = Set.filter (filterFun c) checkHosts
    filterFun :: Cron -> CheckHost -> Bool
    filterFun c (CheckHost (_, i)) = c == cperiod (vc V.! from i)
    cronSet :: Set.Set Cron
    cronSet = foldl (\sc c -> Set.insert (cperiod c) sc) Set.empty vc

    checkHosts :: Set.Set CheckHost
    checkHosts = foldl1 Set.union $
        -- bad magic here, with trigger must be first!!! see Eq and Ord instance for CheckHost
        V.map checkHostsFromTrigger vg <> V.map checkHostsFromGroup vg
    
    checkHostsFromGroup :: Group -> Set.Set CheckHost
    checkHostsFromGroup gr = Set.fromList [ CheckHost (h, c)
                                          | h <- Set.toList $ ghosts gr
                                          , c <- Set.toList $ gchecks gr
                                          ]
    
    checkHostsFromTrigger :: Group -> Set.Set CheckHost
    checkHostsFromTrigger g =
        Set.fromList [ CheckHost (h, c)
                     | h <- Set.toList $ ghosts g
                     , t <- Set.toList $ gtriggers g
                     , c <- tcheck $ vt V.! from t
                     ]

--------------------------------------------------------------------------------------------------
    -- TriggerMap
--------------------------------------------------------------------------------------------------
type TriggerMap = Map CheckHost (Set TriggerId)

triggersMap :: Vector Group -> Vector Trigger -> TriggerMap
triggersMap vg vt =
    let fun :: TriggerHostChecks -> Map CheckHost (Set TriggerId) -> Map CheckHost (Set TriggerId)
        fun x = Map.unionWith Set.union (thcTohcM x)
    in Set.fold fun Map.empty triggerHostChecks
    where
      triggerHostChecks :: Set TriggerHostChecks
      triggerHostChecks =
          let fun (TriggerHost (h,t)) = TriggerHostChecks (h, t, getCheckFromTrigger t)
          in Set.map fun triggerHosts

      triggerHosts:: Set TriggerHost
      triggerHosts =
          let ths g = Set.fromList [ TriggerHost (a, b) | a <- Set.toList (ghosts g), b <- Set.toList (gtriggers g)]
          in V.foldl' Set.union Set.empty $ V.map ths vg

      getCheckFromTrigger :: TriggerId -> [CheckId]
      getCheckFromTrigger ti = tcheck $ vt V.! from ti

      thcTohcM :: TriggerHostChecks -> Map CheckHost (Set TriggerId)
      thcTohcM (TriggerHostChecks (h, th, chs)) = Map.fromList $ Prelude.map (\x -> (CheckHost (h, x), Set.singleton th)) chs




--------------------------------------------------------------------------------------------------

instance Database InfluxDB where
    getData = InfluxDB.getData
    saveData = InfluxDB.saveData
    config = InfluxDB.config



