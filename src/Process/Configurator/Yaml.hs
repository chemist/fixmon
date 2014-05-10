{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Yaml
(parseConfig) where

import           Control.Monad            (mzero)
import           Control.Applicative      (pure, (<$>), (<*>))
import           Control.Arrow            ((&&&))

import           Data.Attoparsec.Text     (parseOnly)
import           Data.HashMap.Strict      (toList)
import qualified Data.Map                 as M
import           Data.Monoid              ((<>))
import qualified Data.Set                 as S
import           Data.Text                (Text)
import           Data.Vector              (Vector, filter, findIndex, foldl,
                                           foldl', foldl1, map, mapM, (!))
import           Data.Yaml                (FromJSON (..), Value (..),
                                           decodeFileEither, (.:), (.:?))
import           System.Cron.Parser       (cronSchedule)

import           Prelude                  hiding (filter, foldl, foldl1, map)

import           Process.Configurator.Dsl (parseTrigger)
import           Types                    (Check (..), CheckHost (..),
                                           CheckId (..), CheckName (..),
                                           Group (..), GroupName (..),
                                           HostId (..), Hostname (..),
                                           Monitoring (..), Status (..),
                                           Trigger (..), TriggerHost (..),
                                           TriggerId (..), TriggerName (..))
import           Types.Cron               (Cron (..))

data ITrigger = ITrigger
  { tname        :: Text
  , tdescription :: Text
  , tcheck       :: Text
  , tresult      :: Text
  } deriving Show

data ICheck = ICheck
  { iname   :: Text
  , iperiod :: Text
  , iparams :: [(Text, Value)]
  } deriving Show

data IGroup = IGroup
  { gname     :: Text
  , ghosts    :: [Hostname]
  , gtriggers :: Maybe [Text]
  , gchecks   :: Maybe [Text]
  } deriving Show

data Config = Config
  { chosts    :: Vector Hostname
  , cgroups   :: Vector IGroup
  , cchecks   :: Vector ICheck
  , ctriggers :: Vector ITrigger
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

instance FromJSON ITrigger where
    parseJSON (Object v) = ITrigger <$>
                           v .: "name" <*>
                           v .: "description" <*>
                           v .: "check" <*> do
                           v .: "result"
    parseJSON _ = mzero

instance FromJSON ICheck where
    parseJSON (Object v) = do
        n <-  v .: "name"
        p <-  v .: "period"
        return $ ICheck n p (toList v)
    parseJSON _ = mzero

instance FromJSON IGroup where
    parseJSON (Object v) = IGroup <$>
                           v .: "name" <*>
                           v .: "hosts" <*>
                           v .:? "triggers" <*>
                           v .:? "checks"
    parseJSON _ = mzero

decodeConf :: FilePath -> IO (Either String Config)
decodeConf fp = conv <$> (decodeFileEither fp)

transformCheck :: ICheck -> Either String Check
transformCheck ch = makeCheck =<< parseOnly cronSchedule (iperiod ch)
  where
    makeCheck cr = return $ Check
      { _checkName = CheckName (iname ch)
      , _period = Cron cr
      , _params = M.filterWithKey (\x _ -> x /= "name" && x /= "period") $ M.map (\(String x) -> x) $ M.fromList $ iparams ch
      }

transformTrigger :: Vector Check -> ITrigger -> Either String Trigger
transformTrigger chs tr = makeTrigger =<< (conv $ parseTrigger (tresult tr))
  where
    makeTrigger tr' = case (findIndex (\c -> _checkName c == CheckName (tcheck tr)) chs) of
                           Nothing -> fail $ "check " ++ (show $ tcheck tr) ++ " not found"
                           Just i -> return $ Trigger (TriggerName (tname tr)) (tdescription tr) (CheckId i) tr'

conv :: Show a => Either a b -> Either String b
conv x = case x of
              Left y -> Left $ show y
              Right y -> Right y

transformGroup :: Vector Check -> Vector Hostname -> Vector Trigger -> IGroup -> Either String Group
transformGroup ch hs tr gr = do
                             let thosts :: Either String [HostId]
                                 thosts = Prelude.mapM funh $ ghosts gr
                                 funh :: Hostname -> Either String HostId
                                 funh h = case (findIndex (== h) hs) of
                                              Nothing -> fail $ "bad hostname in group"
                                              Just i -> return $ HostId i
                                 tchecks :: Maybe [Text] -> Either String [CheckId]
                                 tchecks Nothing = return []
                                 tchecks (Just xs) = Prelude.mapM func xs
                                 func :: Text -> Either String CheckId
                                 func h = case (findIndex (\a -> _checkName a == CheckName h) ch) of
                                               Nothing -> fail "bad check in group"
                                               Just i -> return $ CheckId i
                                 ttriggers :: Maybe [Text] -> Either String [TriggerId]
                                 ttriggers Nothing = return []
                                 ttriggers (Just xs) = Prelude.mapM funt xs
                                 funt :: Text -> Either String TriggerId
                                 funt h = case (findIndex (\a -> _name a == TriggerName h) tr) of
                                               Nothing -> fail "bad trigger in group"
                                               Just i -> return $ TriggerId i
                             hh <- thosts
                             cc <- tchecks $ gchecks gr
                             tt <- ttriggers $ gtriggers gr
                             return $ Group (GroupName $ gname gr) (S.fromList hh) (S.fromList tt) (S.fromList cc)

--------------------------------------------------------------------------------------------------
    -- _checkHosts
--------------------------------------------------------------------------------------------------
checksFromTriggers :: Vector Trigger -> Group -> (S.Set HostId, S.Set CheckId)
checksFromTriggers t g =
  let ch = S.map (\(TriggerId i) -> _check (t ! i)) $ triggers g
  in (hosts g, ch)

checkHosts :: Vector Trigger -> Vector Group -> S.Set CheckHost
checkHosts t m =
  let pairs = map (checksFromTriggers t) m
      pairs' = map (hosts &&& checks) m
      checkHost (x, y) = S.fromList $ [ CheckHost (a, b) | a <-  S.toList x, b <- S.toList y ]
  in foldl1 S.union $ map checkHost (pairs <> pairs')

triggersByCheckHost :: Vector Trigger -> Vector Group -> M.Map CheckHost (S.Set TriggerId)
triggersByCheckHost tv gv =
  let fun :: CheckHost -> Vector Group -> S.Set TriggerId
      fun (CheckHost (a, b)) tv' = foldl S.union S.empty $ map triggers . filter (filterFun a b) $ tv'
      filterFun :: HostId -> CheckId -> Group -> Bool
      filterFun h c g = S.member h (hosts g)  && S.member c (S.map (\(TriggerId x) -> _check (tv ! x)) $ triggers g)
  in M.fromSet (flip fun gv) $ checkHosts tv gv

--------------------------------------------------------------------------------------------------
    -- _periodMap
--------------------------------------------------------------------------------------------------

type PeriodMap = M.Map Cron (S.Set CheckHost)

cronChecks :: Vector Check -> Vector Trigger -> Vector Group -> PeriodMap
cronChecks vc vt vg = M.fromSet fun cronSet
  where checkHosts' = checkHosts vt vg
        fun :: Cron -> S.Set CheckHost
        fun c = S.filter (filterFun c) $ checkHosts'
        filterFun :: Cron -> CheckHost -> Bool
        filterFun c (CheckHost (_, CheckId x)) = c == _period (vc ! x)
        cronSet :: S.Set Cron
        cronSet = foldl (\sc c -> S.insert (_period c) sc) S.empty vc

--------------------------------------------------------------------------------------------------
    -- _status
--------------------------------------------------------------------------------------------------

triggerHosts :: Vector Group -> M.Map TriggerHost Status
triggerHosts vg =
  let ths g = S.fromList $ [ TriggerHost (a, b) | a <- S.toList (hosts g), b <- S.toList (triggers g)]
  in M.fromSet (const (Status True)) $ foldl' S.union S.empty $ map ths vg


configToMonitoring :: Config -> Either String Monitoring
configToMonitoring x = do
    ch <-  Data.Vector.mapM transformCheck $ cchecks x
    tr <-  Data.Vector.mapM (transformTrigger ch) $ ctriggers x
    gg <- Data.Vector.mapM (transformGroup ch (chosts x) tr) $ cgroups x
    let tbch = triggersByCheckHost tr gg
    let crch = cronChecks ch tr gg
    let ths = triggerHosts gg
    return $ Monitoring crch tbch (chosts x) gg tr ch ths

parseConfig :: FilePath -> IO (Either String Monitoring)
parseConfig file = do
    f <-  decodeConf file
    return $ configToMonitoring =<< id f
