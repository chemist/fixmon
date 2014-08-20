{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Process.Configurator.Yaml
(parseConfig) where

import           Control.Applicative      ((<$>), (<*>))
import           Control.Monad            (mzero)

import           Data.Attoparsec.Text     (parseOnly)
import           Data.Either              (lefts, rights)
import           Data.HashMap.Strict      (toList)
import qualified Data.Map                 as M
import           Data.Monoid              ((<>))
import           Data.Scientific
import qualified Data.Set                 as S
import           Data.Text                (Text, concat, unpack)
import           Data.Vector              (Vector, elemIndex, findIndex, foldl,
                                           foldl', foldl1, map, mapM, (!))
import           Data.Vector.Binary       ()
import           Data.Yaml                (FromJSON (..), Value (..),
                                           decodeFileEither, (.:), (.:?))
import           System.Cron.Parser       (cronSchedule)

import           Checks
import           Data.Dynamic
import           Prelude                  hiding (concat, filter, foldl, foldl1,
                                           map)
import qualified Prelude

import           Process.Configurator.Dsl (parseTrigger)
import           Types                    (Check (..), CheckHost (..), CheckId,
                                           CheckName (..), Group (..),
                                           GroupName (..), HostId,
                                           Hostname (..), IntId (..),
                                           Monitoring (..), Status (..),
                                           Trigger (..), TriggerHost (..),
                                           TriggerId, TriggerName (..),
                                           TriggerRaw (..))
import           Types.Cron               (Cron (..))

data ITrigger = ITrigger
  { itname        :: Text
  , itdescription :: Text
  , itcheck       :: [Text]
  , itresult      :: Text
  } deriving Show

data ICheck = ICheck
  { icname   :: Text
  , icperiod :: Text
  , ictype   :: Text
  , icparams :: [(Text, Value)]
  } deriving Show

data IGroup = IGroup
  { igname     :: Text
  , ighosts    :: [Hostname]
  , igtriggers :: Maybe [Text]
  , igchecks   :: Maybe [Text]
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
        {--
        ITrigger <$>
                           v .: "name" <*>
                           v .: "description" <*>
                           (v .: "check" <|> v .: "checks") <*>
                           v .: "result"
                           --}
    parseJSON _ = mzero

instance FromJSON ICheck where
    parseJSON (Object v) = do
        n <-  v .: "name"
        p <-  v .: "period"
        t <-  v .: "type"
        return $ ICheck n p t (toList v)
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

transformCheck :: ICheck -> Either String Check
transformCheck ch = makeCheck =<< conv ("Problem with parse cron in check: " <> icname ch) (parseOnly cronSchedule (icperiod ch))
  where
    makeCheck cr = case M.lookup (ictype ch) checkRoutes of
                       Nothing -> Left $ "unknown check type " ++ unpack (ictype ch)
                       Just f -> f Check { cname = CheckName (icname ch)
                                        , cperiod = Cron cr
                                        , ctype = ictype ch
                                        , cparams = convertCparams $ M.fromList $ icparams ch
                                        }

convertCparams :: M.Map Text Value -> M.Map Text Dynamic
convertCparams = M.map convertValueToDyn
  where
  convertValueToDyn (String x) = toDyn x
  convertValueToDyn (Data.Yaml.Bool x) = toDyn x
  convertValueToDyn (Number x) =
    case floatingOrInteger x of
         Left y -> toDyn (y :: Double)
         Right y -> toDyn (y :: Int)
  convertValueToDyn _ = error "convertValueToDyn, bad type"

transformTrigger :: Vector Check -> ITrigger -> Either String Trigger
transformTrigger chs tr = makeTrigger =<< conv ("Problem with parse result in trigger: " <> itname tr) (parseTrigger (itresult tr))
  where
  makeTrigger :: TriggerRaw Bool -> Either String Trigger
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
                       Just i -> Right $ pId i

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
                                              Just i -> Right $ pId i
                                 tchecks :: Maybe [Text] -> Either String [CheckId]
                                 tchecks Nothing = return []
                                 tchecks (Just xs) = Prelude.mapM func xs
                                 func :: Text -> Either String CheckId
                                 func h = case findIndex (\a -> cname a == CheckName h) ch of
                                               Nothing -> Left "bad check in group"
                                               Just i -> Right $ pId i
                                 ttriggers :: Maybe [Text] -> Either String [TriggerId]
                                 ttriggers Nothing = return []
                                 ttriggers (Just xs) = Prelude.mapM funt xs
                                 funt :: Text -> Either String TriggerId
                                 funt h = case findIndex (\a -> tname a == TriggerName h) tr of
                                               Nothing -> Left "bad trigger in group"
                                               Just i -> Right $ pId i
                             hh <- thosts
                             cc <- tchecks $ igchecks gr
                             tt <- ttriggers $ igtriggers gr
                             return $ Group (GroupName $ igname gr) (S.fromList hh) (S.fromList tt) (S.fromList cc)

--------------------------------------------------------------------------------------------------
    -- _checkHosts
--------------------------------------------------------------------------------------------------
checkHosts :: Vector Trigger -> Vector Group -> S.Set CheckHost
checkHosts vtrigger vgroup = foldl1 S.union $
    map checkHostsFromGroup vgroup <> map (checkHostsFromTrigger vtrigger) vgroup

checkHostsFromGroup :: Group -> S.Set CheckHost
checkHostsFromGroup gr = S.fromList [ CheckHost (h, c, Nothing)
                                    | h <- S.toList $ ghosts gr
                                    , c <- S.toList $ gchecks gr
                                    ]

checkHostsFromTrigger :: Vector Trigger -> Group -> S.Set CheckHost
checkHostsFromTrigger vt vg =
    S.fromList [ CheckHost (h, c, Just t)
               | h <- S.toList $ ghosts vg
               , t <- S.toList $ gtriggers vg
               , c <- tcheck $ vt ! unId t
               ]
--------------------------------------------------------------------------------------------------
    -- _periodMap
--------------------------------------------------------------------------------------------------

type PeriodMap = M.Map Cron (S.Set CheckHost)

cronChecks :: Vector Check -> Vector Trigger -> Vector Group -> PeriodMap
cronChecks vc vt vg = M.fromSet fun cronSet
  where checkHosts' = checkHosts vt vg
        fun :: Cron -> S.Set CheckHost
        fun c = S.filter (filterFun c) checkHosts'
        filterFun :: Cron -> CheckHost -> Bool
        filterFun c (CheckHost (_, i, _)) = c == cperiod (vc ! unId i)
        cronSet :: S.Set Cron
        cronSet = foldl (\sc c -> S.insert (cperiod c) sc) S.empty vc

--------------------------------------------------------------------------------------------------
    -- _status
--------------------------------------------------------------------------------------------------

triggerHosts :: Vector Group -> M.Map TriggerHost Status
triggerHosts vg =
  let ths g = S.fromList [ TriggerHost (a, b) | a <- S.toList (ghosts g), b <- S.toList (gtriggers g)]
  in M.fromSet (const (Status True)) $ foldl' S.union S.empty $ map ths vg


configToMonitoring :: Config -> Either String Monitoring
configToMonitoring x = do
    ch <-  Data.Vector.mapM transformCheck $ cchecks x
    tr <-  Data.Vector.mapM (transformTrigger ch) $ ctriggers x
    gg <- Data.Vector.mapM (transformGroup ch (chosts x) tr) $ cgroups x
    let crch = cronChecks ch tr gg
    let ths = triggerHosts gg
    return $ Monitoring crch (chosts x) gg tr ch ths

parseConfig :: FilePath -> IO (Either String Monitoring)
parseConfig file = do
    f <-  decodeConf file
    return $ configToMonitoring =<< f
