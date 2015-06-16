{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Configurator.Yaml
(parseConfig) where

import           Control.Monad        (mzero)

import           Data.Attoparsec.Text (parseOnly)
import           Data.Either          (lefts, rights)
import           Data.HashMap.Strict  (toList)
import qualified Data.Map.Strict      as M
import           Data.Monoid          ((<>))
import           Data.Scientific
import qualified Data.Set             as S
import           Data.Text            (Text, concat, unpack)
import           Data.Vector          (Vector, elemIndex, findIndex, mapM)
import           Data.Vector.Binary   ()
import           Data.Yaml            (FromJSON (..), Value (..),
                                       decodeFileEither, object, (.!=), (.:),
                                       (.:?))
import           System.Cron.Parser   (cronSchedule)

import           Check.Snmp.Snmp      (Rules)
import           Checks
import           Prelude              hiding (concat, filter, foldl, foldl1,
                                       map)
import qualified Prelude

import           Configurator.Dsl     (parseTrigger)
import           Storage.InfluxDB     (InfluxDB (..))
import           Types                (Check (..), CheckId, CheckName (..),
                                       Convert (..), Exp, Group (..),
                                       GroupName (..), HostId, Hostname (..),
                                       Monitoring (..), Trigger (..), TriggerId,
                                       TriggerName (..), mkMonitoring)
import           Types.Cron           (Cron (..))


data ITrigger = ITrigger
  { itname        :: !Text
  , itdescription :: !Text
  , itcheck       :: ![Text]
  , itresult      :: !Text
  } deriving Show

data ICheck = ICheck
  { icname   :: !Text
  , icperiod :: !Text
  , ictype   :: !Text
  , icparams :: ![(Text, Value)]
  } deriving Show

data IGroup = IGroup
  { igname     :: !Text
  , ighosts    :: ![Hostname]
  , igtriggers :: !(Maybe [Text])
  , igchecks   :: !(Maybe [Text])
  } deriving Show

data ISystem = ISystem
  { idb :: InfluxDB
  } deriving Show

data Config = Config
  { chosts    :: !(Vector Hostname)
  , cgroups   :: !(Vector IGroup)
  , cchecks   :: !(Vector ICheck)
  , ctriggers :: !(Vector ITrigger)
  , csystem   :: !ISystem
  } deriving Show

instance FromJSON Config where
    parseJSON (Object v) = Config        <$>
                           v .: "hosts"  <*>
                           v .: "groups" <*>
                           v .: "checks" <*>
                           v .: "triggers" <*>
                           v .: "system"
    parseJSON _ = mzero

instance FromJSON ISystem where
    parseJSON (Object v) = do
        d <-  v .: "database"
        return $ ISystem d
    parseJSON _ = mzero

instance FromJSON InfluxDB where
    parseJSON (Object v) = do
        b <- v .:? "database" .!= "fixmon"
        u <- v .:? "user" .!= "fixmon"
        p <- v .:? "password" .!= "fixmon"
        h <- v .:? "host"  .!= "fixmon"
        port' <- v .:? "port"  .!= 8086
        s <- v .:? "ssl" .!= False
        return $ InfluxDB b u p h port' s
    parseJSON _ = mzero

instance FromJSON ITrigger where
    parseJSON (Object v) = do
        n <- v .: "name"
        d <- v .: "description"
        c <- v .:? "check"
        ch <- v .:? "checks"
        r <- v .: "result"
        let ch' = case (c, ch) of
                       (Just x, Just xs) -> x : xs
                       (Just x, Nothing) -> [x]
                       (Nothing, Just xs) -> xs
                       (Nothing, Nothing) -> mzero
        return $ ITrigger n d ch' r
    parseJSON _ = mzero

instance FromJSON ICheck where
    parseJSON (Object v) = do
        n <-  v .: "name"
        p <-  v .: "period"
        t <-  v .: "type"
        return $ ICheck n p t (clean $ toList v)
        where
        clean = Prelude.filter (\(x,_) -> x /= "name" && x /= "period" && x /= "type" && x /= "snmp")
    parseJSON _ = mzero

instance FromJSON IGroup where
    parseJSON (Object v) = IGroup <$>
                           v .: "name" <*>
                           v .: "hosts" <*>
                           v .:? "triggers" <*>
                           v .:? "checks"
    parseJSON _ = mzero

decodeConf :: FilePath -> IO (Either String Config)
decodeConf fp = conv "Problem with parse yaml format: " <$> decodeFileEither fp

transformCheck :: Rules -> ICheck -> Either String Check
transformCheck rules ch = makeCheck =<< conv ("Problem with parse cron in check: " <> icname ch) (parseOnly cronSchedule (icperiod ch))
  where
    makeCheck cr = case M.lookup (ictype ch) (checkRoutes rules) of
                       Nothing -> Left $ "unknown check type " ++ unpack (ictype ch)
                       Just f -> f Check { cname = CheckName (icname ch)
                                        , chost = Hostname ""
                                        , cperiod = Cron cr
                                        , ctype = ictype ch
                                        , cparams = convertCparams $ icparams ch
                                        }

convertCparams :: [(Text, Value)] -> Value
convertCparams = object . Prelude.map convertValueToDyn
  where
  convertValueToDyn (c, String x) = (c, to x)
  convertValueToDyn (c, Data.Yaml.Bool x) = (c, to x)
  convertValueToDyn (c, Number x) =
    case floatingOrInteger x of
         Left y -> (c, to (y :: Double))
         Right y -> (c, to (y :: Int))
  convertValueToDyn c = error $ "convertValueToDyn, bad type" ++ show c

transformTrigger :: Vector Check -> ITrigger -> Either String Trigger
transformTrigger chs tr = makeTrigger =<< conv ("Problem with parse result in trigger: " <> itname tr) (parseTrigger (itresult tr))
  where
  makeTrigger :: Exp -> Either String Trigger
  makeTrigger tr' = do
      ch <- checks''
      return $ Trigger (TriggerName (itname tr)) (itdescription tr) ch tr'
  checks'' :: Either String [CheckId]
  checks'' = let l = concat . lefts . checks' . itcheck $ tr
                 r = rights . checks' . itcheck $ tr
             in case l of
                    "" -> Right r
                    e -> Left $ unpack e
  checks' :: [Text] -> [Either Text CheckId]
  checks' = Prelude.map
              $ \x -> case findIndex (\c -> cname c == CheckName x) chs of
                       Nothing -> Left $ "check " <> x  <> " not found"
                       Just i -> Right $ to i

conv :: Show a => Text -> Either a b -> Either String b
conv tag x = case x of
              Left y -> Left $ unpack tag <> " | error: " <> show y
              Right y -> Right y

transformGroup :: Vector Check -> Vector Hostname -> Vector Trigger -> IGroup -> Either String Group
transformGroup ch hs tr gr = do
                             let thosts :: Either String [HostId]
                                 thosts = Prelude.mapM funh $ ighosts gr
                                 funh :: Hostname -> Either String HostId
                                 funh h = case elemIndex h hs of
                                              Nothing -> Left "bad hostname in group"
                                              Just i -> Right $ to i
                                 tchecks :: Maybe [Text] -> Either String [CheckId]
                                 tchecks Nothing = return []
                                 tchecks (Just xs) = Prelude.mapM func xs
                                 func :: Text -> Either String CheckId
                                 func h = case findIndex (\a -> cname a == CheckName h) ch of
                                               Nothing -> Left "bad check in group"
                                               Just i -> Right $ to i
                                 ttriggers :: Maybe [Text] -> Either String [TriggerId]
                                 ttriggers Nothing = return []
                                 ttriggers (Just xs) = Prelude.mapM funt xs
                                 funt :: Text -> Either String TriggerId
                                 funt h = case findIndex (\a -> tname a == TriggerName h) tr of
                                               Nothing -> Left "bad trigger in group"
                                               Just i -> Right $ to i
                             hh <- thosts
                             cc <- tchecks $ igchecks gr
                             tt <- ttriggers $ igtriggers gr
                             return $ Group (GroupName $ igname gr) (S.fromList hh) (S.fromList tt) (S.fromList cc)

--------------------------------------------------------------------------------------------------
    -- _status
--------------------------------------------------------------------------------------------------

{--
triggerHosts :: Vector Group -> M.Map TriggerHost Status
triggerHosts vg =
  let ths g = S.fromList [ TriggerHost (a, b) | a <- S.toList (ghosts g), b <- S.toList (gtriggers g)]
  in M.fromSet (const (Status True)) $ foldl' S.union S.empty $ map ths vg
  --}


configToMonitoring :: Rules -> Config -> Either String Monitoring
configToMonitoring r x = do
    ch <-  Data.Vector.mapM (transformCheck r) $ cchecks x
    tr <-  Data.Vector.mapM (transformTrigger ch) $ ctriggers x
    gg <- Data.Vector.mapM (transformGroup ch (chosts x) tr) $ cgroups x
    return $ mkMonitoring (chosts x) ch tr gg r (idb $ csystem x)

parseConfig :: FilePath -> FilePath -> IO (Either String Monitoring)
parseConfig file snmpConf = do
    f <-  decodeConf file
    s <-  conv "cant parse snmp rules" <$> decodeFileEither snmpConf
    return $ do
       x <- s
       y <- f
       configToMonitoring x y
