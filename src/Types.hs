{-# LANGUAGE OverloadedStrings #-}
module Types
( HostId
, Hostname(..)
-- * Триггеры
, TriggerId
, TriggerName(..)
, Trigger(..)
, TriggerHost(..)
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
, Monitoring(..)
-- , Log(..)
, IntId(..)
, routeCheck'
, module Types.Dynamic
, Database(..)
-- * monads
-- * error
-- * for tests
-- , testHttp
) where

import           Types.Check
import           Types.Cron
import           Types.Dynamic
import           Types.Shared

class Database db where
    getData :: db -> Table -> Fun -> IO Dyn
    saveData :: db -> [(Hostname, Complex)] -> IO ()
    config :: db


