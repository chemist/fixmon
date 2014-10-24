{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
module Main where

import Pipes 
import Pipes.Concurrent
import Control.Concurrent.Async
import System.Cron
import Control.Monad.State
import Data.Map.Strict (Map, elems, filterWithKey, lookup)
import Data.Set (Set, unions)
import Data.Time.Clock
import Control.Concurrent (threadDelay, killThread)
import Process.Configurator.Yaml
import Control.Applicative
import Data.Vector ((!?), (!))
import System.IO 
import Control.Exception
import Data.Monoid ((<>))
import Data.Maybe
import Data.Text (pack)
import Data.IORef
import Prelude hiding (lookup)
-- import System.Mem (performGC)

import Types ( 
               DBException, saveData, Trigger
             , Hostname(..), Complex(..), toDyn, Counter(..) 
             , TriggerName(..), tname, Database, chost, ctype
             , getData, Env(..), Monitoring(..), CheckHost(..)
             , config, Cron(..), Check(..), Table(..)
             , tdescription, tresult, unId, eval
             )
import Checks (routes)
import Storage.InfluxDB (InfluxDB)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Right m <- parseConfig "fixmon.yaml"
    (taskO, taskI) <- spawn Unbounded
    (saverO, saverI) <- spawn Unbounded
    (triggerO, triggerI) <- spawn Unbounded
    lock <- newIORef True
    p1 <- forkIO $ (evalStateT . runEffect)  
                  (cron >-> taskMaker >-> toOutput taskO) 
                  m
    p2 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    p3 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    p4 <- forkIO $ (evalStateT . runSST . runEffect ) 
                  (fromInput saverI >-> saver >-> toOutput triggerO) 
                  $ SQ (config::InfluxDB) [] [] lock 
    p5 <- forkIO $ (evalStateT . runEffect)
                  (fromInput triggerI >-> checkTrigger (config :: InfluxDB) >-> shower)
                  m
    _ <- getLine :: IO String
    mapM_ killThread [p1,p2,p3,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

data Command = Reload ST

type ST = Map Cron (Set CheckHost)

type FixmonST = StateT Monitoring IO 
type Task = (Check, Maybe Trigger)

cron :: Producer CheckHost FixmonST ()
cron = forever $ do
    liftIO $ threadDelay $ seconds 10
    now <- liftIO getCurrentTime
    st <- _periodMap <$> get
    let tasks = unions . elems $ filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
    each tasks 

taskMaker :: Pipe CheckHost Task FixmonST ()
taskMaker = forever $ do
    Monitoring _ hosts _ triggers checks _ <- get
    CheckHost (h, c, mt) <- await
    let check = checks ! unId c
        host = hosts ! unId h
        trigger = maybe Nothing (\x -> triggers !? unId x) mt
    yield (check { chost = host }, trigger)

checkTrigger :: Database db => db -> Pipe (Hostname, Trigger) (Hostname, Complex) FixmonST ()
checkTrigger db = forever $ do
    (Hostname h,t) <- await
    r <- liftIO $ eval (Env (getData db (Table h))) (tresult t)
    saveTriggers db [(Hostname h, [triggerToComplex t r])]
    yield (Hostname h, triggerToComplex t r)
    where
      saveTriggers db queue = liftIO $ saveData db queue `catch` \(e :: DBException) -> print e

data CounterType = State
                 | Message

triggerToComplex :: Trigger -> Either DBException Bool -> Complex
triggerToComplex tr (Left e) = Complex [ (toCounter tr State , toDyn False)
                                       , (toCounter tr Message, toDyn $ pack $ show e)
                                       ]
triggerToComplex tr (Right True) = Complex [ (toCounter tr State, toDyn True) ]
triggerToComplex tr (Right False) = Complex [ (toCounter tr State, toDyn False)
                                            , (toCounter tr Message, toDyn $ tdescription tr)
                                            ]

toCounter :: Trigger -> CounterType -> Counter
toCounter tr State = let TriggerName tn = tname tr
                     in Counter $ "trigger." <> tn <> ".status"
toCounter tr Message = let TriggerName tn = tname tr
                       in Counter $ "trigger." <> tn <> ".message"
 

tasker :: Pipe Task (Hostname, Maybe Trigger, [Complex]) IO ()
tasker = forever $ do
    (c, mt) <- await
    let ch = lookup (ctype c) routes
        host = chost c
    r <- liftIO $ maybe notFound (doCheck' c) ch
    yield (host, mt, r)
    where
       notFound = return [Complex [("_status_", toDyn False)]]
       doCheck' check doCheck'' = do
         checkResult <- try $ doCheck'' check
         case checkResult of
           Left (_ :: SomeException) -> return [Complex [("_status_", toDyn False)]]
           Right r -> return r

newtype SaverST a b = SST {runSST :: StateT (SaveQueue a) IO b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (SaveQueue a))

data SaveQueue a = SQ
  { database :: a
  , queueCheck :: [(Hostname, [Complex])]
  , queueTrigger :: [(Hostname, Trigger)]
  , locker :: IORef Bool
  }



saver :: Database db => Pipe (Hostname, Maybe Trigger, [Complex]) (Hostname, Trigger) (SaverST db) ()
saver = forever $ do
    timer =<< locker <$> get
    add
    saveChecks
    sendTriggers
    again =<< locker <$> get
    where
      saveChecks = do
          ch <- queueCheck <$> get
          db <- database <$> get 
          liftIO $ saveData db ch `catch` (\(e :: DBException) -> print e)
      sendTriggers = do
          t <- queueTrigger <$> get
          each t
      again lock = do
          liftIO $ atomicWriteIORef lock True
          db <- database <$> get
          put $ SQ db [] [] lock
      timer lock = liftIO $ void $ async $ do
          threadDelay 1000000
          atomicWriteIORef lock False
      add = do
          (h, mt, c) <- await
          modify $ \x -> x { queueCheck = (h,c) : queueCheck x }
          when (isJust mt) $ modify $ \x -> x { queueTrigger = (h,fromJust mt) : queueTrigger x }
          l <- locker <$> get
          l' <- liftIO $ readIORef l
          when l' $ add



shower :: (Show a, MonadIO m) => Consumer a m ()
shower = forever $ await >>= (\x -> liftIO $ print x)

