{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Checks               (routes)
import           Configurator.Yaml
import           Control.Applicative
import           Control.Concurrent   (killThread, threadDelay)
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict  as HM
import           Data.Map.Strict      (Map, lookup)
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Data.Monoid          ((<>))
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Time.Clock
import           Data.Yaml
import           Pipes
import           Pipes.Concurrent
import           Prelude              hiding (log, lookup)
import           System.IO
import           Types                (Check (..), Complex, Convert (..),
                                       Counter, Counter (..), DBException (..),
                                       Database, Dyn, Env (..), Fun (..),
                                       TaskResult(..), 
                                       ErrorMSG,
                                       Hostname (..), Monitoring (..),
                                       Period (..), Task (..), Trigger (..),
                                       countersFromExp, ctype, eval, getData,
                                       saveData)

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

tasker :: Pipe Task TaskResult (Fixmon ()) ()
tasker = forever $ do
    task'@(Task _ vCheck _) <- await
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
          queue <-  get
          db <- storage <$> ask
          liftIO $ saveData db queue `catch` (\(e :: DBException) -> print $ "saver: " <> show e)
      add = do
          vTaskResult <- await
          modify $ (:) vTaskResult
          yield vTaskResult
          s <- length <$> get
          if s > 1 then return () else add

taskResultToComplex :: TaskResult -> Complex
taskResultToComplex (TaskResult _ _ _ _ _ x) = x -- TODO: convert

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
  | otherwise = do
      let cache = buildCache vHost (findKeys vTriggers) vComplex
      updatedCache <-  state $ \s -> (M.union cache s, M.union cache s)
      db <- storage <$> ask
      let environment = createEnv db (from vHost) updatedCache
      result <- forM (S.toList vTriggers) $ \x -> do
          res <- liftIO $ eval environment $ tresult x
          case res of
               Left e -> return $ TriggerResult vHost vCheck x (vError <> show e) False
               Right b -> return $ TriggerResult vHost vCheck x vError b
      each $ result
--      liftIO $ print vStatus
--      liftIO $ print vComplex
--      liftIO $ print updatedCache
--      liftIO $ print result

findKeys :: Set Trigger -> Set Counter
findKeys = S.foldr  fun S.empty -- (countersFromExp . tresult)
  where
    fun vTrigger = S.union $ (S.fromList . countersFromExp . tresult) vTrigger

buildCache :: Hostname -> Set Counter -> Complex -> Cache
buildCache vHostname vKeys (Object hm) =
    let idInObject = HM.lookup "id" hm
        keysInComplex = S.map (\x -> (vHostname, x)) $ S.filter (\vCounter -> (String <$> cId vCounter) == idInObject) vKeys
        cache = M.filter (\y -> y /= Null) $ M.fromSet (\(_, x) -> fromMaybe Null $ HM.lookup (cName x) hm) keysInComplex
    in cache
buildCache vHostname vKeys (Array hm) = foldl (\m o -> M.union m (buildCache vHostname vKeys o)) M.empty hm
buildCache _ _ _ = error "bad object in buildCache"

createEnv :: Database db => db -> T.Text -> Cache -> Env
createEnv db t cash = Env (createEnv' t)
  where
  createEnv' hostname (EnvValFun c) =
    let v = M.lookup (Hostname hostname, c) cash
    in case v of
            Nothing -> createEnv' t (LastFun c (Count 1))
            Just r -> return r
  createEnv' _ f  = getData db t f



