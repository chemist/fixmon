{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Configurator.Yaml
(parseConfig) where

import           Control.Applicative      ((<$>), (<*>))
import           Control.Monad            (mzero)

import           Data.Attoparsec.Text     (parseOnly)
import           Data.Either              (lefts, rights)
import           Data.HashMap.Strict      (toList)
import qualified Data.Map.Strict          as M
import           Data.Monoid              ((<>))
import           Data.Scientific
import qualified Data.Set                 as S
import           Data.Text                (Text, concat, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector              (Vector, elemIndex, findIndex, foldl,
                                           foldl', foldl1, map, mapM, (!))
import           Data.Vector.Binary       ()
import           Data.Yaml                (FromJSON (..), Value (..),
                                           decodeFileEither, (.:), (.:?), (.!=))
import           System.Cron.Parser       (cronSchedule)

import           Checks
import           Prelude                  hiding (concat, filter, foldl, foldl1,
                                           map)
import qualified Prelude

import           Configurator.Dsl         (parseTrigger)
import           Types                    (Check (..), CheckHost (..), CheckId,
                                           CheckName (..), Group (..),
                                           GroupName (..), HostId, Dyn(..), Convert(..),
                                           Hostname (..), Counter(..),
                                           Monitoring (..), Status (..), TriggerHostChecks(..),
                                           Trigger (..), TriggerHost (..),Exp,
                                           TriggerId, TriggerName (..))
import           Types.Cron               (Cron (..))
import qualified Network.Protocol.Snmp as Snmp
import qualified Network.Snmp.Client as Snmp
import Storage.InfluxDB (InfluxDB(..))

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
  , icsnmp   :: Maybe Snmp.Config
  , icparams :: ![(Text, Value)]
  } deriving Show

data IGroup = IGroup
  { igname     :: !Text
  , ighosts    :: ![Hostname]
  , igtriggers :: !(Maybe [Text])
  , igchecks   :: !(Maybe [Text])
  } deriving Show

data ISystem = ISystem
  { isnmp :: Snmp.Config
  , idb   :: InfluxDB
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
        s <- v .: "snmp"
        d <-  v .: "database"
        return $ ISystem s d 
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

instance FromJSON Snmp.Config where
    parseJSON (Object v) = do
        ver <- v .:? "version" .!= (3 :: Int)
        case ver of
             3 -> parseVersion3 
             2 -> parseVersion2 
             _ -> error "bad snmp version"
        where
            parseVersion3 = do
                (sn :: Text) <- v .: "sequrityName"
                pt <- v .:? "privType" .!= "DES"
                pa <- v .:? "privAuth" .!= "AuthPriv"
                at <- v .:? "authType" .!= "MD5"
                p <-  v .:? "port"     .!= "161"
                t <-  v .:? "timeout"  .!= (5 :: Int)
                ap <- v .: "authPass"
                pp <- v .:? "privPass" .!= ap
                return $ Snmp.ConfigV3 "" 
                                       p 
                                       (t * 1000000)
                                       (encodeUtf8 sn) 
                                       (encodeUtf8 ap)
                                       (encodeUtf8 pp) 
                                       (encodePrivAuth pa) 
                                       "" 
                                       (encodeAuthType at) 
                                       (encodePrivType pt)
            parseVersion2 = do
                (sn :: Text) <- v .: "community" .!= "public"
                p <-  v .:? "port"     .!= "161"
                t <-  v .:? "timeout"  .!= (5 :: Int)
                return $ Snmp.ConfigV2 "" p (t * 1000000) (Snmp.Community $ encodeUtf8 sn)

    parseJSON _ = mzero


encodePrivType :: Text -> Snmp.PrivType
encodePrivType "AES" = Snmp.AES
encodePrivType "DES" = Snmp.DES
encodePrivType _ = error "bad priv type"

encodeAuthType :: Text -> Snmp.AuthType
encodeAuthType "MD" = Snmp.MD5
encodeAuthType "MD5" = Snmp.MD5
encodeAuthType "SHA" = Snmp.SHA
encodeAuthType _ = error "bad priv type"

encodePrivAuth :: Text -> Snmp.PrivAuth
encodePrivAuth "NoAuthNoPriv" = Snmp.NoAuthNoPriv
encodePrivAuth "AuthNoPriv" = Snmp.AuthNoPriv
encodePrivAuth "AuthPriv" = Snmp.AuthPriv
encodePrivAuth _ = error "bad priv auth"




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
        c <-  v .:? "snmp"
        return $ ICheck n p t c (clean $ toList v)
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

transformCheck :: ICheck -> Either String Check
transformCheck ch = makeCheck =<< conv ("Problem with parse cron in check: " <> icname ch) (parseOnly cronSchedule (icperiod ch))
  where
    makeCheck cr = case M.lookup (ictype ch) checkRoutes of
                       Nothing -> Left $ "unknown check type " ++ unpack (ictype ch)
                       Just f -> f Check { cname = CheckName (icname ch)
                                        , chost = Hostname ""
                                        , cperiod = Cron cr
                                        , ctype = ictype ch
                                        , csnmp = icsnmp ch
                                        , cparams = convertCparams $ icparams ch
                                        }

convertCparams :: [(Text, Value)] -> [(Counter, Dyn)]
convertCparams = Prelude.map convertValueToDyn
  where
  convertValueToDyn (c, String x) = (Counter c, to x)
  convertValueToDyn (c, Data.Yaml.Bool x) = (Counter c, to x)
  convertValueToDyn (c, Number x) =
    case floatingOrInteger x of
         Left y -> (Counter c, to (y :: Double))
         Right y -> (Counter c, to (y :: Int))
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
    -- _checkHosts
--------------------------------------------------------------------------------------------------
checkHosts :: Vector Trigger -> Vector Group -> S.Set CheckHost
checkHosts vtrigger vgroup = foldl1 S.union $
    -- bad magic here, with trigger must be first!!! see Eq and Ord instance for CheckHost
    map (checkHostsFromTrigger vtrigger) vgroup <> map checkHostsFromGroup vgroup 

checkHostsFromGroup :: Group -> S.Set CheckHost
checkHostsFromGroup gr = S.fromList [ CheckHost (h, c)
                                    | h <- S.toList $ ghosts gr
                                    , c <- S.toList $ gchecks gr
                                    ]

checkHostsFromTrigger :: Vector Trigger -> Group -> S.Set CheckHost
checkHostsFromTrigger vt vg =
    S.fromList [ CheckHost (h, c)
               | h <- S.toList $ ghosts vg
               , t <- S.toList $ gtriggers vg
               , c <- tcheck $ vt ! from t
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
        filterFun c (CheckHost (_, i)) = c == cperiod (vc ! from i)
        cronSet :: S.Set Cron
        cronSet = foldl (\sc c -> S.insert (cperiod c) sc) S.empty vc

--------------------------------------------------------------------------------------------------
    -- _thc
--------------------------------------------------------------------------------------------------

getCheckFromTrigger :: Vector Trigger -> TriggerId -> [CheckId]
getCheckFromTrigger vt ti = tcheck $ vt ! from ti

triggerHostChecks :: Vector Group -> Vector Trigger -> S.Set TriggerHostChecks
triggerHostChecks vg vt =
    let sth :: S.Set TriggerHost
        sth = M.keysSet . triggerHosts $ vg
        fun (TriggerHost (h,t)) = TriggerHostChecks (h, t, getCheckFromTrigger vt t)
    in S.map fun sth

thcTohcM :: TriggerHostChecks -> M.Map CheckHost (S.Set TriggerId)
thcTohcM (TriggerHostChecks (h, th, chs)) = M.fromList $ Prelude.map (\x -> (CheckHost (h, x), S.singleton th)) chs

triggersMap :: Vector Group -> Vector Trigger -> M.Map CheckHost (S.Set TriggerId)
triggersMap vg vt =
    let s = triggerHostChecks vg vt
        fun :: TriggerHostChecks -> M.Map CheckHost (S.Set TriggerId) -> M.Map CheckHost (S.Set TriggerId)
        fun x = M.unionWith S.union (thcTohcM x) 
    in S.fold fun M.empty s


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
    let trhcs = triggersMap gg tr
    return $ Monitoring crch (chosts x) gg tr ch ths trhcs (isnmp $ csystem x) (idb $ csystem x)

parseConfig :: FilePath -> IO (Either String Monitoring)
parseConfig file = do
    f <-  decodeConf file
    return $ configToMonitoring =<< f
