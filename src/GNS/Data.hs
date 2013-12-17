{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE KindSignatures  #-}
module GNS.Data where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Attoparsec.Number
import           Data.ByteString        (ByteString)
import           Data.Map               (Map)
import           Data.Set               (Set)
import           Data.String
import           Data.Text
import           Data.Time              (UTCTime)
import           Data.Typeable
import           Data.Word
import           Data.Yaml
import qualified Data.Yaml              as Y
import           Debug.Trace
import           Network.HTTP.Types     hiding (Status)
import           System.Cron

newtype TriggerId = TriggerId Int deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map CronSchedule (Set TriggerId)
 , _triggers  :: Map TriggerId Trigger
 , _status    :: Map TriggerId Status
 } deriving Show

newtype CheckName = CheckName Text

instance Show CheckName where
    show (CheckName x) = show x

data Trigger = Trigger
  { _name        :: Text
  , _period      :: CronSchedule
  , _description :: Text
  , _check       :: CheckName
  , _result      :: TriggerFun
  } deriving Show

newtype Status = Status { unStatus :: Bool } deriving Show

newtype TriggerFun = TriggerFun (Complex -> Status)

instance Show TriggerFun where
     show x = show "trigger fun here"

data Group = Group
 { name     :: Text
 , hosts    :: [Hostname]
 , triggers :: [Text]
 } deriving Show

type Hostname = Text

data Config = Config
  { hosts'    :: ! [Hostname]
  , groups'   :: ! [Group]
  , triggers' :: ! [Trigger]
  , checks'   :: ! [Check]
  } deriving Show

----------------------------------------------------------------------------------------------------
-- check type
----------------------------------------------------------------------------------------------------
data Return = CI Number | CB Bool

instance Show Return where
    show (CI x) = show x
    show (CB x) = show x

instance Typeable Return where
    typeOf (CI x) = typeOf x
    typeOf (CB x) = typeOf x

instance Eq Return where
    (==) a b | typeOf a == typeOf b =
                case (a,b) of
                     (CI x, CI y) -> x == y
                     (CB x, CB y) -> x == y
            | otherwise = throw $ TypeError $ "you try do " ++ (show $ typeOf a) ++ " == " ++ (show $ typeOf b)

    (/=) a b | typeOf a == typeOf b =
                case (a,b) of
                     (CI x, CI y) -> x /= y
                     (CB x, CB y) -> x /= y
            | otherwise = throw $ TypeError $ "you try do " ++ (show $ typeOf a) ++ " == " ++ (show $ typeOf b)

instance Ord Return where
    compare a b | typeOf a == typeOf b =
                    case (a,b) of
                         (CI x, CI y) -> compare x y
                         (CB x, CB y) -> compare x y
                | otherwise = throw $ TypeError $ "you try compare " ++ (show $ typeOf a) ++ " and " ++ (show $ typeOf b)

data TypeError = TypeError String deriving (Show, Typeable)

instance Exception TypeError

newtype Name = Name Text deriving (Show, Eq, Ord)

instance IsString Name where
    fromString x = Name . pack $ x

newtype Complex = Complex (Map Name Return) deriving Show

data Check = Check { _checkName :: CheckName 
                   , _params :: Map Text Text
                   } deriving Show

