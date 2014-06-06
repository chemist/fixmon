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
-- * Группы
, GroupName(..)
, GroupId
, Group(..)
-- * Общее
, Cron(..)
, Status(..)
, Monitoring(..)
, Complex(..)
, PError(..)
, Log(..)
, StartOptions(..)
-- * monads
-- * error
, TypeError
, IntId(..)
-- * for tests
, testHttp
) where

import Types.Shared 
import Types.Cron 
import Types.DslTypes 

import           Data.Map    (fromList)
import           System.Cron

testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", "http://ya.ru") ])
