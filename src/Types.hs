{-# LANGUAGE OverloadedStrings #-}
module Types
( HostId
, Hostname(..)
, ToAny(..)
-- * Триггеры
, TriggerId
, TriggerName(..)
, Trigger(..)
, TriggerHost(..)
, TriggerRaw(..)
, Any(..)
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
, Complex(..)
, Log(..)
, StartOptions(..)
, IntId(..)
, Tag
, textType
, intType
, boolType
, routeCheck'
-- * monads
-- * error
, TypeError
-- * for tests
, testHttp
) where

import           Types.Check
import           Types.Cron
import           Types.DslTypes
import           Types.Shared

import           Data.Dynamic
import           Data.Text (Text)
import           Data.Map       (fromList)
import           System.Cron

testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", toDyn ("http://ya.ru" :: Text)) ])

