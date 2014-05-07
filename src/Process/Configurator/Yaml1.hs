{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Yaml1 where

import Data.Yaml
import Data.Vector
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as DH
import Data.Text (pack, Text)
import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Control.Monad (mzero)
import Debug.Trace

import Process.Configurator.Dsl
import Types.Cron
import System.Cron.Parser
import Types (Hostname(..), TriggerRaw)

data Trigger = Trigger
  { name :: Text
  , description :: Text
  , check :: Text
  , result :: TriggerRaw Bool
  } deriving Show

data Check = Check 
  { checkName :: Text
  , period :: Cron
  , params :: [(Text, Value)]
  } deriving Show

data Group = Group 
  { gname :: Text
  , ghosts :: [Hostname]
  , gtriggers :: Maybe [Text]
  , gchecks   :: Maybe [Text]
  } deriving Show

data Config = Config 
  { hosts :: Vector Hostname
  , groups :: Vector Group
  , checks :: Vector Check
  , triggers :: Vector Trigger
  } deriving Show

instance FromJSON Config where
    parseJSON (Object v) = Config        <$>
                           v .: "hosts"  <*>
                           v .: "groups" <*>
                           v .: "checks" <*>
                           v .: "triggers" 
    parseJSON _ = mzero 


instance FromJSON Hostname where
    parseJSON (String x) = pure $ Hostname x
    parseJSON _ = mzero

instance FromJSON Trigger where
    parseJSON (Object v) = Trigger <$>
                           v .: "name" <*>
                           v .: "description" <*>
                           v .: "check" <*> do
                               x <- v .: "result"
                               either (const mzero) pure $ parseTrigger x
    parseJSON x = trace (show x) $ mzero

instance FromJSON Check where
    parseJSON (Object v) = do
        n <-  v .: "name" 
        p <-  v .: "period"
        return $ Check n p (DH.toList v)
    parseJSON _ = mzero 

instance FromJSON Cron where
    parseJSON (String x) = either (const mzero) (pure . Cron) $ parseOnly cronSchedule x
    parseJSON _ = mzero

instance FromJSON Group where
    parseJSON (Object v) = Group <$> 
                           v .: "name" <*> 
                           v .: "hosts" <*> 
                           v .:? "triggers" <*> 
                           v .:? "checks"
    parseJSON _ = mzero
