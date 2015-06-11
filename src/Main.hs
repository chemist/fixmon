{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Checks                   (routes)
import           Configurator.Yaml
import           Control.Applicative
import           Control.Concurrent       (killThread, threadDelay)
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict      as HM
import           Data.Map.Strict          (Map, lookup)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Monoid              ((<>))
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Text                as T
import           Data.Time.Clock
import           Data.Yaml
import           Pipes
import           Pipes.Concurrent
import           Prelude                  hiding (log, lookup)
import           System.IO
import           Types                    (Check (..), CheckHost (..), CheckId,
                                           Complex, Convert (..), Counter,
                                           Counter (..), DBException (..),
                                           Database, Dyn, Env (..), Fun (..),
                                           HostId, Hostname (..),
                                           Monitoring (..), Period (..), Prefix,
                                           Status (..), Task (..), Trigger (..),
                                           TriggerHost (..), TriggerId, ctype,
                                           eval, getData, saveData)

import           Debug.Trace
-- PEN assigned for fixmon
-- Prefix: iso.org.dod.internet.private.enterprise (1.3.6.1.4.1)
-- PEN: 44729

main :: IO ()
main = do
    trace "start" $ hSetBuffering stdout LineBuffering
    m <- either (error . show) id <$> parseConfig "fixmon.yaml" "snmp.yaml"
    (saverO, saverI) <- spawn unbounded
    (triggerO, triggerI) <- spawn unbounded
    p1 <- forkIO $ run m (cron >-> log "cron >-> tasker" >-> tasker >-> log "tasker >-> saverO" >->  toOutput saverO) ()
    p4 <- forkIO $ run m (fromInput saverI  >-> saver >-> log "saver >-> triggerO" >-> toOutput triggerO) []
    p5 <- forkIO $ run m (fromInput triggerI >-> log "triggerI >-> checkTrigger" >-> checkTrigger >-> log "checkTrigger >-> shower" >-> shower) M.empty
    _ <- getLine :: IO String
    mapM_ killThread [p1,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

type Cache = Map (Hostname, Counter) Dyn

newtype Fixmon a b = Fixmon { runFixmon :: ReaderT Monitoring (StateT a IO) b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState a, MonadReader Monitoring)

run :: forall s a. Monitoring -> Effect (Fixmon s) a -> s -> IO a
run m = evalStateT . (flip runReaderT m) . runFixmon . runEffect

log :: (Monad m, Show a, MonadIO m) => String -> Pipe a a m ()
log _prefix = forever $ do
   x <- await
   -- liftIO $ putStr $ _prefix ++ ": " ++ show x ++ "\n\n"
   yield x

cron :: Producer Task (Fixmon ()) ()
cron = forever $ do
    liftIO $ threadDelay $ seconds 10
    now <- liftIO getCurrentTime
    st <- ask
    each (task st now)

type ErrorMSG = String
data TaskResult = TaskResult Hostname Check (Set Trigger) ErrorMSG Bool Complex deriving Show

tasker :: Pipe Task TaskResult (Fixmon ()) ()
tasker = forever $ do
    task'@(Task _ vCheck _) <- await
    liftIO $ print task'
    rules <- snmpRules <$> ask
    let mfCheck = lookup (ctype vCheck) (routes rules)
    yield =<< liftIO (maybe (notFound task') (doCheck' task') mfCheck)
    where
       notFound :: Task -> IO TaskResult
       notFound (Task vHost vCheck vTrigger) = return $ TaskResult vHost vCheck vTrigger "check type not found" False Null
       doCheck' :: Task -> (Check -> IO Complex) -> IO TaskResult
       doCheck' (Task vHost vCheck vTrigger) doCheck'' = do
         checkResult <- try $ doCheck'' vCheck
         case checkResult of
           Left (h :: SomeException) -> return $ TaskResult vHost vCheck vTrigger (show h) False Null
           Right r -> return $  TaskResult vHost vCheck vTrigger "" True r

saver :: Pipe TaskResult TaskResult (Fixmon [TaskResult]) ()
saver = forever $ do
    add
    saveChecks
    put []
    where
      saveChecks = do
          queue <- map taskResultToComplex <$> get
          db <- storage <$> ask
          liftIO $ saveData db queue `catch` (\(e :: DBException) -> print $ "saver: " <> show e)
      add = do
          vTaskResult <- await
          modify $ (:) vTaskResult
          liftIO $ print "~~~~~~~~~~~~~~~~~~~~~~"
          liftIO $ print vTaskResult
          liftIO $ print "~~~~~~~~~~~~~~~~~~~~~~"
          yield vTaskResult
          s <- length <$> get
          if s > 1 then return () else add

taskResultToComplex :: TaskResult -> Complex
taskResultToComplex (TaskResult _ _ _ _ _ x) = x -- TODO: convert

{--
getTrigger :: (Functor m, Monad m, MonadReader Monitoring m) => TriggerId -> m Trigger
getTrigger i = (\x -> triggers x ! from i) <$> ask

getCountersById :: (MonadIO m, Functor m, Monad m, MonadReader Monitoring m) => Set TriggerId -> m IKeys
getCountersById vSetTriggerId = do
    vTriggers <- triggers <$> ask
    let a = Prelude.concat $ S.map (\x -> countersFromExp (tresult (vTriggers ! from x))) vSetTriggerId
        addId x = (fromMaybe "" (cId x), S.singleton x)
    return $ M.fromListWith S.union $ map addId a
    --}

shower :: MonadIO m => Consumer TriggerResult m ()
shower = forever $ do
    triggerResult <- await
    liftIO . print $ triggerResult

data TriggerResult = TriggerResult Hostname Check Trigger ErrorMSG Bool deriving (Show, Eq, Ord)

checkTrigger :: Pipe TaskResult TriggerResult (Fixmon Cache) ()
checkTrigger = forever $ work =<< await

work :: TaskResult -> Pipe TaskResult TriggerResult (Fixmon Cache) ()
work (TaskResult vHost vCheck vTriggers vError vStatus vComplex)
  | S.null vTriggers = return ()
  | not vStatus = each $ S.map (\x -> TriggerResult vHost vCheck x vError vStatus) vTriggers
  | otherwise = each $ S.map (\x -> TriggerResult vHost vCheck x vError vStatus) vTriggers

{--
work (Array vComplexBody) = V.mapM_ work vComplexBody
work vComplex@(Object vComplexBody) = do
    vTriggerMap <- undefined -- triggerMap <$> ask
    let Just (Bool isCheckSuccess) = HM.lookup "_success_" vComplexBody
        Just vCheckId = from <$> HM.lookup "_check_id_" vComplexBody :: Maybe CheckId
        Just vHostname = from <$> HM.lookup "_host_name_" vComplexBody :: Maybe Hostname
        Just vHostId = from <$> HM.lookup "_host_id_" vComplexBody :: Maybe HostId
        Just (String vErrorMessage) = HM.lookup "_error_message_" vComplexBody
        Just (String vPrefix) = HM.lookup "_check_prefix_" vComplexBody
        mvSetTriggerId = lookup (CheckHost (vHostId, vCheckId)) vTriggerMap
        vTriggerIdList = S.toList $ fromJust mvSetTriggerId
    when (isJust mvSetTriggerId && isCheckSuccess) $ do
      vIKeys <-  undefined -- getCountersById (fromJust mvSetTriggerId)
      when (isCombined vComplex $ M.keysSet vIKeys) $ do
        let toCache = makeCache vHostname vIKeys vPrefix vComplex
        modify (M.union toCache)
        cache <- get
        db <- storage <$> ask
        triggerValue <- undefined -- Prelude.mapM getTrigger vTriggerIdList
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
--}
type IKeys = Map T.Text (Set Counter)


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



