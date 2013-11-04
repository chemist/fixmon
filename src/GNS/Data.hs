{-# LANGUAGE OverloadedStrings #-}
module GNS.Data where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text
import Data.Time (UTCTime)
import Network.HTTP.Types
import System.Cron
import Data.Typeable
import Data.Yaml
import qualified Data.Yaml as Y
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Number
import Data.ByteString (ByteString)
import Debug.Trace

newtype TriggerId = TriggerId Int deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map CronSchedule (Set TriggerId)
 , _triggers :: Map TriggerId Trigger
 , _status :: Map TriggerId Result
 } deriving Show

type CheckName = Text

data Trigger = Trigger 
  { _name :: Text
  , _period :: CronSchedule
  , _check :: CheckName
  , _description :: Text
  } deriving Show


data TriggerStatus = Good | Bad deriving Show

data Result = Result
  { _trigger :: Text
  , _result :: TriggerStatus
  , _time :: UTCTime
  } deriving Show

data Return = CI Int | CB Bool | Or [Return] | Not Return deriving (Show)

data Check = Shell 
  { _checkName :: Text
  , _sh :: Text
  , _good :: Return
  }         
             | HttpByStatus
  { _checkName :: Text
  , _url :: Text
  , _good :: Return
  } 
  deriving (Show)

data Group = Group
 { name::Text
 , hosts::[Hostname]
 , triggers::[Text]
 } deriving Show

type Hostname = Text

data Config = Config
  { groups' :: [Group]
  , triggers' :: [Trigger]
  , checks' :: [Check]
  } deriving Show


{-
groups:
  - name: web
    hosts:
      - ubank.su
    triggers:
      processing
  - name: db
    hosts:
      - db01.ubank.su
      - db02.ubank.su
    triggers:
      - oracle
  - name: app
    hosts:
      - app01.ubank.su
      - app02.ubank.su
    triggers:
      - java
  - name: all-linux
    hosts: *
    triggers:
      - disk-root

triggers: 
  - name: processing
    period: * * * * *
    check: http-processing
    description: Проверка процессинга по админке
  - name: oracle
    period: * * * * *
    check: base-check
    description: Проверка базы
  - name: java
    period: * * * * *
    check: process-java
    description: Проверяем что java работает
  - name: disk
    period: * * * * *
    check: disk-root
    description: Проверка свободного места

checks:
  - name: http-processing
    type: httpByStatus
    url: http://localhost/ok
    good: 
      - 200 Ok
      - 303 Ok
  - name: base-check
    type: shell
    good: ok
  - name: disk-root
    type: shell
    good: 100
  - name: process-java
    type: shell
    good: true

    -}
