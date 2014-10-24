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
-- import           Network.HTTP.Conduit (Request)
-- import qualified Network.HTTP.Types.Status as H
-- import Data.Typeable
--

class Database db where
    getData :: db -> Table -> Fun -> IO Dyn
    saveData :: db -> [(Hostname, [Complex])] -> IO ()
    config :: db

