{-# LANGUAGE OverloadedStrings #-}
module GNS.Data where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Number
import           Data.ByteString        (ByteString)
import           Data.Map               (Map)
import           Data.Set               (Set)
import           Data.Text
import           Data.Time              (UTCTime)
import           Data.Typeable
import           Data.Yaml
import qualified Data.Yaml              as Y
import           Debug.Trace
import           Network.HTTP.Types
import           System.Cron

newtype TriggerId = TriggerId Int deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map CronSchedule (Set TriggerId)
 , _triggers  :: Map TriggerId Trigger
 , _status    :: Map TriggerId Result
 } deriving Show

type CheckName = Text

data Trigger = Trigger
  { _name        :: Text
  , _period      :: CronSchedule
  , _check       :: CheckName
  , _description :: Text
  } deriving Show


data TriggerStatus = Good | Bad deriving Show

data Result = Result
  { _trigger :: Text
  , _result  :: TriggerStatus
  , _time    :: UTCTime
  } deriving Show

data Return = CI Int | CB Bool | Or [Return] | Not Return deriving (Show)

data Check = Shell
  { _checkName :: Text
  , _sh        :: Text
  , _good      :: Return
  }
           | HttpByStatus
  { _checkName :: Text
  , _url       :: Text
  , _good      :: Return
  }
  deriving (Show)

data Group = Group
 { name     :: Text
 , hosts    :: [Hostname]
 , triggers :: [Text]
 } deriving Show

type Hostname = Text

data Config = Config
  { groups'   :: [Group]
  , triggers' :: [Trigger]
  , checks'   :: [Check]
  } deriving Show

