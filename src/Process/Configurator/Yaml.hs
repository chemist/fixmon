{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Yaml  where

import           Process.Configurator.Dsl (parseTrigger)
import           Types

import           Control.Applicative      ((<$>))
import           Control.Arrow
import           Control.Exception
import           Control.Monad.Error
import           Control.Monad.RWS
import           Data.Attoparsec.Text     (parseOnly)
import           Data.ByteString          (ByteString)
import           Data.Either
import           Data.Map                 (Map, fromList, toList, (!))
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                hiding (empty, filter, group, head,
                                           map, zip)
import           Data.Yaml.Syck
import           Prelude                  hiding (not, or)
import           System.Cron.Parser

inv :: Map CheckId Check -> Map CheckName CheckId
inv x = fromList $ map (\(a,b) -> (_checkName b,a)) $ toList x

inv' :: Map TriggerId Trigger -> Map TriggerName (TriggerId, Trigger)
inv' x = fromList $ map (\(a,b) -> (_name b, (a,b))) $ toList x

inv'' :: Map HostId Hostname -> Map Hostname HostId
inv'' x = fromList $ map (\(a,b) -> (b,a)) $ toList x

checkConf :: Monitoring -> Monitoring
checkConf x = x { _checkHost = allMonit x
                , _periodMap = allCron x (cronSet $ _checks x)
                , _status = allTriggerHost x
                }

type IdChecks = Map CheckId Check
type CronChecks = Map Cron (Set CheckHost)

allCron :: Monitoring -> Set Cron -> CronChecks
allCron mon s = Map.fromSet (fun mon) s
  where checks' = _checks mon
        fun :: Monitoring -> Cron -> Set CheckHost
        fun m c = Set.filter (\(CheckHost (_,x)) -> c == _period (checks' ! x )) $ allCheckHost m

cronSet :: IdChecks -> Set Cron
cronSet x = Map.foldl fun Set.empty x
  where fun :: Set Cron -> Check -> Set Cron
        fun a b = Set.insert (_period b) a

checkByGroup :: Monitoring -> Group -> (Set HostId, Set CheckId)
checkByGroup m g = let fun :: Monitoring -> TriggerId -> CheckId
                       fun mon t = _check $ _triggers mon Map.! t
                       tc = Set.map (fun m) $ triggers g
                       y = checks g `Set.union` tc
                   in (hosts g, y)


triggerHost :: Group -> Set TriggerHost
triggerHost g = Set.fromList $ [ TriggerHost (a,b)
                               | a <- Set.toList $ hosts g
                               , b <- Set.toList $ triggers g
                               ]

allTriggerHost :: Monitoring -> Map TriggerHost Status
allTriggerHost m = Map.fromSet (\_ -> Status True) $ Set.unions $ map triggerHost $ _groups m

checkHost :: (Set HostId, Set CheckId) -> Set CheckHost
checkHost (x, y) = Set.fromList $ [ CheckHost (a,b)
                                  | a <- Set.toList x
                                  , b <- Set.toList y
                                  ]

allCheckHost :: Monitoring -> Set CheckHost
allCheckHost m = let a = map (checkByGroup m) $ _groups m
                 in Set.unions $ map checkHost a

allMonit :: Monitoring -> Map CheckHost (Set TriggerId)
allMonit m  = let ach = allCheckHost m
                  fun :: Monitoring -> CheckHost -> Set TriggerId
                  fun m' (CheckHost (a, b)) = Set.unions $ map triggers $ filter (\x -> Set.member a (hosts x) && Set.member b (checks x) ) $ _groups m'
              in Map.fromSet (fun m) ach

parseConfig :: Gns ()
parseConfig = do
    s <- ask
    st <- get
    result <- liftIO $ (try $ do
        EMap root <- n_elem <$> parseYamlFile (config s)
        che <- mapM runErrorT $ unpackChecks root
        let che' = fromList $ zip (map CheckId [1 .. ]) (rights che)
        tr <- mapM runErrorT $ unpackTriggers root (inv che')
        let tr' = fromList $ zip (map TriggerId [1 .. ]) (rights tr)
            hs = unpackHosts root
            gr = unpackGroups (inv che') tr' (inv'' hs) root
        return $ st { _hosts = hs
                    , _groups = gr
                    , _triggers = tr'
                    , _checks = che'
                    }) :: Gns (Either SomeException Monitoring)
    either (\e -> throwError $ "cant read config " ++ show e) fun result
    where
    fun :: Monitoring -> Gns ()
    fun f = tell "success read and parse config\n" >> put (checkConf f)

-----------------------------------------------------------------------------------------
-- helpers
-----------------------------------------------------------------------------------------

unElem :: YamlNode -> Text
unElem = unpackEStr . n_elem

unNode :: [(YamlNode, YamlNode)] -> ByteString -> YamlNode
unNode root node = case (filter (\(y,_) -> n_elem y == EStr node) root) of
                       [(_, x)] -> x
                       _ -> nilNode { n_elem = ESeq [] }

unEN :: [(YamlNode, YamlNode)] -> ByteString -> Text
unEN x = unElem . unNode x

-----------------------------------------------------------------------------------------
-- hosts
-----------------------------------------------------------------------------------------

unpackHosts :: [(YamlNode, YamlNode)] -> Map HostId Hostname
unpackHosts root = fromList $ zip (map HostId [1 .. ]) $ map Hostname $ unpackESeq $ unNode root "hosts"

unpackESeq :: YamlNode -> [Text]
unpackESeq x = let ESeq l = n_elem x
               in map unElem l

unpackEStr :: YamlElem -> Text
unpackEStr (EStr z) = pack $ unpackBuf z
unpackEStr _ = throw $ PError "error in unpackEStr, must be EStr"

-----------------------------------------------------------------------------------------
-- groups
-----------------------------------------------------------------------------------------

unpackGroups :: Map CheckName CheckId -> Map TriggerId Trigger -> Map Hostname HostId -> [(YamlNode, YamlNode)] -> [Group]
unpackGroups cc tt hs root = let ESeq l = n_elem $ unNode root "groups"
                             in map (unpackGroup cc tt hs) l

unpackGroup :: Map CheckName CheckId -> Map TriggerId Trigger -> Map Hostname HostId -> YamlNode -> Group
unpackGroup cc tt hs group = let EMap ls = n_elem group
                                 triggers'' = Set.fromList $ map ((\x -> fst $ (inv' tt) ! x) . TriggerName) $ unpackESeq $ unNode ls "triggers"
                                 checks'' = Set.fromList $ map ((cc !) . CheckName) $ unpackESeq $ unNode ls "checks"
                                 triggerCheck :: Set TriggerId -> Set CheckId
                                 triggerCheck = Set.map (\x -> _check $ tt ! x)
                             in Group
                                  { name = GroupName $ unEN ls "name"
                                  , hosts = Set.fromList $ map ((hs !) . Hostname) $ unpackESeq $ unNode ls "hosts"
                                  , triggers = triggers''
                                  , checks  = checks'' `Set.union` triggerCheck triggers''
                                  }

-----------------------------------------------------------------------------------------
-- triggers
-----------------------------------------------------------------------------------------

unpackTriggers :: Monad m => [(YamlNode, YamlNode)] -> Map CheckName CheckId -> [ErrorT PError m Trigger]
unpackTriggers root mc = let ESeq l = n_elem $ unNode root "triggers"
                         in map (unpackTrigger mc) l

unpackTrigger :: Monad m => Map CheckName CheckId -> YamlNode -> ErrorT PError m Trigger
unpackTrigger mc trigger = let EMap ls = n_elem trigger
                               pp = parseTrigger $ unElem $ unNode ls "result"
                           in case pp of
                                Left e -> throw $ PError $ "cant parse trigger " ++ show e
                                Right y -> return $ Trigger
                                    { _name = TriggerName $ unEN ls "name"
                                    , _check = mc ! (CheckName $ unEN ls "check")
                                    , _description = unEN ls "description"
                                    , _result = y
                                    }

-----------------------------------------------------------------------------------------
-- checks
-----------------------------------------------------------------------------------------

unpackChecks :: Monad m => [(YamlNode, YamlNode)] -> [ErrorT PError m Check]
unpackChecks root = let ESeq l = n_elem $ unNode root "checks"
                    in map unpackCheck  l

unpackCheck :: Monad m => YamlNode -> ErrorT PError m Check
unpackCheck check = let EMap ls = n_elem check
                        checkName = unNode ls "name"
                        period = parseOnly cronSchedule $  unEN ls "period"
                    in case period of
                            Left e -> throw $ PError $ "cant parse cron schedule " ++ show e
                            Right r -> return $ Check
                              { _checkName = CheckName $ unElem checkName
                              , _period = Cron r
                              , _params = fromList $ map (unElem *** unElem) $ filter (\(x, _) -> n_elem x /= EStr "name" || n_elem x /= EStr "period") ls
                              }

