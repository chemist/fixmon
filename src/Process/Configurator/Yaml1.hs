{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Yaml1 where

import Data.Yaml
import Data.Vector hiding ((++))
import Data.HashMap.Strict hiding (map)
import qualified Data.HashMap.Strict as DH
import Data.Text (pack, Text)
import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Control.Monad (mzero)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Text.Peggy.Prim (ParseError)
import Prelude hiding (map)

import Process.Configurator.Dsl
import Types.Cron
import System.Cron.Parser
import Types (Hostname(..), TriggerRaw)
import qualified Types as T

data Trigger = Trigger
  { name :: Text
  , description :: Text
  , check :: Text
  , result :: Text
  } deriving Show

data Check = Check 
  { checkName :: Text
  , period :: Text
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
                           v .: "result"
    parseJSON _ = mzero

instance FromJSON Check where
    parseJSON (Object v) = do
        n <-  v .: "name" 
        p <-  v .: "period"
        return $ Check n p (DH.toList v)
    parseJSON _ = mzero 

instance FromJSON Group where
    parseJSON (Object v) = Group <$> 
                           v .: "name" <*> 
                           v .: "hosts" <*> 
                           v .:? "triggers" <*> 
                           v .:? "checks"
    parseJSON _ = mzero

decodeConf :: FilePath -> IO (Either ParseException Config)
decodeConf fp = decodeFileEither fp

transformCheck :: Check -> Either String T.Check
transformCheck ch = makeCheck =<< parseOnly cronSchedule (period ch)
  where
    makeCheck cr = return $ T.Check 
      { T._checkName = T.CheckName (checkName ch)
      , T._period = Cron cr
      , T._params = M.filterWithKey (\x _ -> x /= "name" && x /= "period") $ M.map (\(String x) -> x) $ M.fromList $ params ch
      }

transformTrigger :: Vector T.Check -> Trigger -> Either String T.Trigger
transformTrigger chs tr = makeTrigger =<< (conv $ parseTrigger (result tr))
  where
    makeTrigger tr' = case (findIndex (\c -> T._checkName c == T.CheckName (check tr)) chs) of
                           Nothing -> fail $ "check " ++ (show $ check tr) ++ " not found"
                           Just i -> return $ T.Trigger (T.TriggerName (name tr)) (description tr) (T.CheckId i) tr'

conv :: Either ParseError (TriggerRaw Bool) -> Either String (TriggerRaw Bool)
conv x = case x of
              Left y -> Left $ show y
              Right y -> Right y

transformGroup :: Vector T.Check -> Vector T.Hostname -> Vector T.Trigger -> Group -> Either String T.Group
transformGroup ch hs tr gr = do
                             let thosts :: Either String [T.HostId]
                                 thosts = Prelude.mapM funh $ ghosts gr
                                 funh :: Hostname -> Either String T.HostId
                                 funh h = case (findIndex (== h) hs) of
                                              Nothing -> fail $ "bad hostname in group"
                                              Just i -> return $ T.HostId i
                                 tchecks :: Maybe [Text] -> Either String [T.CheckId]
                                 tchecks Nothing = return [] 
                                 tchecks (Just xs) = Prelude.mapM func xs
                                 func :: Text -> Either String T.CheckId
                                 func h = case (findIndex (\a -> T._checkName a == T.CheckName h) ch) of
                                               Nothing -> fail "bad check in group"
                                               Just i -> return $ T.CheckId i
                                 ttriggers :: Maybe [Text] -> Either String [T.TriggerId]
                                 ttriggers Nothing = return []
                                 ttriggers (Just xs) = Prelude.mapM funt xs
                                 funt :: Text -> Either String T.TriggerId
                                 funt h = case (findIndex (\a -> T._name a == T.TriggerName h) tr) of
                                               Nothing -> fail "bad trigger in group"
                                               Just i -> return $ T.TriggerId i
                             hh <- thosts
                             cc <- tchecks $ gchecks gr
                             tt <- ttriggers $ gtriggers gr
                             return $ T.Group (T.GroupName $ gname gr) (S.fromList hh) (S.fromList tt) (S.fromList cc)

dc x = do
    ch <-  Data.Vector.mapM transformCheck $ checks x
    tr <-  Data.Vector.mapM (transformTrigger ch) $ triggers x
    gg <- Data.Vector.mapM (transformGroup ch (hosts x) tr) $ groups x
    return (ch,tr, gg)
