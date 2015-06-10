{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Configurator.Yaml
import           Control.Applicative
import           Control.Concurrent   (killThread, threadDelay)
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict  as HM
import           Data.Map.Strict      (Map, elems, filterWithKey, lookup)
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Data.Monoid          ((<>))
import           Data.Set             (Set, unions)
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import qualified Data.Text.IO         as T
import           Data.Time.Clock
import           Data.Vector          ((!))
import qualified Data.Vector as V
import           Data.Yaml
import qualified Data.Aeson.Encode.Pretty as P
import Data.ByteString.Lazy (toStrict)
import           Pipes
import           Pipes.Concurrent
import           Prelude              hiding (log, lookup)
import           System.Cron
import           System.IO
-- import qualified Prelude
-- import Debug.Trace
-- import System.Mem (performGC)

import           Checks               (routes)
import           Types                (Check (..), CheckHost (..), CheckId,
                                       Complex, Convert (..), Counter,
                                       Cron (..), DBException (..), Database,
                                       Dyn, Env (..), Fun (..), HostId,
                                       Hostname (..), Monitoring (..), Counter(..),
                                       Period (..), Status (..), Prefix,
                                       Trigger (..), TriggerHost (..),
                                       TriggerId, chost, countersFromExp, ctype,
                                       eval, getData, saveData)

import Debug.Trace
-- PEN assigned for fixmon
-- Prefix: iso.org.dod.internet.private.enterprise (1.3.6.1.4.1)
-- PEN: 44729

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    m <- either (error . show) id <$> parseConfig "fixmon.yaml" "snmp.yaml"
    (taskO, taskI) <- spawn unbounded
    (saverO, saverI) <- spawn unbounded
    (triggerO, triggerI) <- spawn unbounded
    p1 <- forkIO $ run m (cron >-> log "cron >-> taskMaker" >-> taskMaker >-> log "taskMaker >-> taskO" >->  toOutput taskO) ()
    p2 <- forkIO $ run m (fromInput taskI >-> log "taskI >-> tasker" >-> tasker >-> log "tasker >-> saverO" >-> toOutput saverO) ()
--    p3 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    p4 <- forkIO $ run m (fromInput saverI  >-> saver >-> log "saver >-> triggerO" >-> toOutput triggerO) []
    p5 <- forkIO $ run m (fromInput triggerI >-> log "triggerI >-> checkTrigger" >-> checkTrigger >-> log "checkTrigger >-> shower" >-> shower) M.empty
    _ <- getLine :: IO String
    mapM_ killThread [p1,p2,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

type Cache = Map (Hostname, Counter) Dyn

data Task = Task Check CheckHost deriving (Show)

newtype Fixmon a b = Fixmon { runFixmon :: ReaderT Monitoring (StateT a IO) b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState a, MonadReader Monitoring)

run :: forall s a. Monitoring -> Effect (Fixmon s) a -> s -> IO a
run m = evalStateT . (flip runReaderT m) . runFixmon . runEffect


cron :: Producer CheckHost (Fixmon ()) ()
cron = forever $ do
    liftIO $ threadDelay $ seconds 10
    now <- liftIO getCurrentTime
    st <- periodMap <$> ask
    let tasks = unions . elems $ filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
    each tasks

log :: (Monad m, Show a, MonadIO m) => String -> Pipe a a m ()
log _prefix = forever $ do
   x <- await
   -- liftIO $ putStr $ _prefix ++ ": " ++ show x ++ "\n\n"
   yield x

taskMaker :: Pipe CheckHost Task (Fixmon ()) ()
taskMaker = forever $ do
    Monitoring _ hosts' _ _ checks' _ snmp' _ _ <- ask
    CheckHost (h, c) <- await
    let check = checks' ! from c
        host = hosts' ! from h
    yield $ Task (check { chost = host, csnmp = Just $ fromMaybe snmp' (csnmp check)}) (CheckHost (h,c))

tasker :: Pipe Task Complex (Fixmon ()) ()
tasker = forever $ do
    Task vCheck vCheckHost <- await
    vHost <- getHost vCheckHost
    rules <- snmpRules <$> ask
    let mfCheck = lookup (ctype vCheck) (routes rules)
    yield =<< liftIO (maybe (notFound vCheck vCheckHost vHost) (doCheck' vCheck vCheckHost vHost) mfCheck)
    where
       notFound :: Check -> CheckHost -> Hostname -> IO Complex
       notFound vCheck (CheckHost (vHostId, vCheckId)) vHost = return $ object $
         [ "_success_" .= Bool False
         , "_host_id_"   .= (to vHostId :: Value)
         , "_host_name_"   .= (to vHost :: Value)
         , "_check_name_"  .= (to (cname vCheck) :: Value)
         , "_check_id_"  .= (to vCheckId :: Value)
         , "_error_message_" .= String "check type not found"
         , "_check_prefix_" .= (String $ ctype vCheck)
         ]
       doCheck' :: Check -> CheckHost -> Hostname -> (Check -> IO Complex) -> IO Complex
       doCheck' vCheck (CheckHost (vHostId, vCheckId)) vHost doCheck'' = do
         checkResult <- try $ doCheck'' vCheck
         case checkResult of
           Left (h :: SomeException) -> return $ object $
             [ "_success_" .= Bool False
             , "_host_id_"   .= (to vHostId :: Value)
             , "_host_name_"   .= (to vHost :: Value)
             , "_check_name_"  .= (to (cname vCheck) :: Value)
             , "_check_id_"  .= (to vCheckId :: Value)
             , "_error_message_" .= (String $ T.pack $ show h)
             , "_check_prefix_" .= (String $ ctype vCheck)
             ]
           Right r -> return $ unionObject r $ object $
             [ "_success_" .= Bool True
             , "_host_id_"    .= (to vHostId :: Value)
             , "_host_name_"  .= (to vHost :: Value)
             , "_check_name_"  .= (to (cname vCheck) :: Value)
             , "_check_id_"   .= (to vCheckId :: Value)
             , "_check_prefix_" .= (String $ ctype vCheck)
             ]


unionObject :: Value -> Value -> Value
unionObject (Object x) (Object y) = Object $ HM.union x y
unionObject (Array xm) (Object x) = Array $ V.map (\(Object y) -> Object $ HM.union x y) xm
unionObject _ _ = error "unionObject only for Object"

saver :: Pipe Complex Complex (Fixmon [Complex]) ()
saver = forever $ do
    add
    saveChecks
    put []
    where
      saveChecks = do
          queue <- get
          db <- storage <$> ask
          liftIO $ saveData db queue `catch` (\(e :: DBException) -> print $ "saver: " <> show e)
      add = do
          vComplex <- await
          modify $ (:) vComplex
          yield vComplex
          s <- length <$> get
          if s > 1 then return () else add

getHost :: (Functor m, Monad m, MonadReader Monitoring m)  => CheckHost -> m Hostname
getHost (CheckHost (i, _)) = (\x -> hosts x ! from i) <$> ask

getTrigger :: (Functor m, Monad m, MonadReader Monitoring m) => TriggerId -> m Trigger
getTrigger i = (\x -> triggers x ! from i) <$> ask

getCountersById :: (MonadIO m, Functor m, Monad m, MonadReader Monitoring m) => Set TriggerId -> m IKeys
getCountersById vSetTriggerId = do
    vTriggers <- triggers <$> ask
    let a = Prelude.concat $ S.map (\x -> countersFromExp (tresult (vTriggers ! from x))) vSetTriggerId
        addId x = (fromMaybe "" (cId x), S.singleton x)
    return $ M.fromListWith S.union $ map addId a

shower :: MonadIO m => Consumer (TriggerHost, Status) m ()
shower = forever $ do
    (vTriggerHost, vStatus) <- await
    liftIO . T.putStr $ (T.pack $ show vTriggerHost) <> " " <> toText vStatus <> "\n"
    where
      toText Ok = "Success"
      toText (Bad e) = "Problem\n" <> e
      toText (SomethingWrong e) = "Wrong\n" <> e

checkTrigger :: Pipe Complex (TriggerHost, Status) (Fixmon Cache) ()
checkTrigger = forever $ work =<< await

work :: Complex -> Pipe Complex (TriggerHost, Status) (Fixmon Cache) ()
work (Array vComplexBody) = V.mapM_ work vComplexBody
work vComplex@(Object vComplexBody) = do
    vTriggerMap <- triggerMap <$> ask
    let Just (Bool isCheckSuccess) = HM.lookup "_success_" vComplexBody
        Just vCheckId = from <$> HM.lookup "_check_id_" vComplexBody :: Maybe CheckId
        Just vHostname = from <$> HM.lookup "_host_name_" vComplexBody :: Maybe Hostname
        Just vHostId = from <$> HM.lookup "_host_id_" vComplexBody :: Maybe HostId
        Just (String vErrorMessage) = HM.lookup "_error_message_" vComplexBody
        Just (String vPrefix) = HM.lookup "_check_prefix_" vComplexBody
        mvSetTriggerId = lookup (CheckHost (vHostId, vCheckId)) vTriggerMap
        vTriggerIdList = S.toList $ fromJust mvSetTriggerId
    when (isJust mvSetTriggerId && isCheckSuccess) $ do
      vIKeys <- getCountersById (fromJust mvSetTriggerId)
      when (isCombined vComplex $ M.keysSet vIKeys) $ do
        let toCache = makeCache vHostname vIKeys vPrefix vComplex
        modify (M.union toCache)
        cache <- get
        db <- storage <$> ask
        triggerValue <- Prelude.mapM getTrigger vTriggerIdList
        result <- liftIO $ mapM (\x -> eitherToStatus vComplex <$> eval (createEnv db (from vHostname) cache ) (tresult x)) triggerValue
        each $ toTriggerHost vHostId vTriggerIdList result
    when (isJust mvSetTriggerId && not isCheckSuccess) $ do
      each $ toTriggerHost vHostId vTriggerIdList (repeat (SomethingWrong vErrorMessage))
    return ()
    where
      eitherToStatus _ (Left e) = Bad $ T.pack $ show e
      eitherToStatus _ (Right True) = Ok
      eitherToStatus o (Right False) = Bad $ "trigger say problem\n" <> decodeUtf8 (toStrict $ P.encodePretty o)
work _ = error "work: bad complex"

type IKeys = Map T.Text (Set Counter)

toTriggerHost :: HostId -> [TriggerId] -> [Status] -> [(TriggerHost, Status)]
toTriggerHost hid tids rs = map (\(t, r) -> (TriggerHost (hid, t), r)) $  zip tids rs

createEnv :: Database db => db -> T.Text -> Cache -> Env
createEnv db t cash = Env (createEnv' t)
  where
  createEnv' hostname (EnvValFun c) =
    let v = M.lookup (Hostname hostname, c) cash
    in case v of
            Nothing -> createEnv' t (LastFun c (Count 1))
            Just r -> return r
  createEnv' _ f  = getData db t f

-- combined check when Check -> [Complex]
isCombined :: Complex -> Set T.Text -> Bool
isCombined (Object xs) ids =
    let id' = HM.lookup "id" xs
    in case id' of
            Just (String id'') -> S.member id'' ids
            _ -> False
isCombined _ _ = error "bad complex in isCombined"

makeCache :: Hostname -> IKeys -> Prefix -> Complex -> Cache
makeCache vHost vIKeys vPrefix (Object vComplexBody) =
    let Just (String vValue) = HM.lookup "id" vComplexBody
        ikeys = fromJust . flip M.lookup vIKeys
        addPrefix (x,y) = ((vHost, Counter (Just vValue) vPrefix x), y)
        filterOnlyUsed (x, _) = S.member x (ikeys vValue)
        asList = HM.toList vComplexBody
    in undefined  -- M.fromList $ map addPrefix . filter filterOnlyUsed $  asList
makeCache _ _ _ _ = error "bad Complex in removeUnusedFromCombined"



