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
import Data.Maybe
import Data.IORef
import Prelude hiding (lookup)
-- import System.Mem (performGC)

import Types
import Checks
import Storage.InfluxDB (InfluxDB)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Right m <- parseConfig "fixmon.yaml"
    (taskO, taskI) <- spawn Unbounded
    (saverO, saverI) <- spawn Unbounded
    (triggerO, triggerI) <- spawn Unbounded
    lock <- newIORef True
    p1 <- forkIO $ evalStateT (cronP taskO) m
    p2 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    p3 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    -- p4 <- forkIO $ runEffect $ fromInput saverI >-> shower
    p4 <- forkIO $ (evalStateT . runSST)  (saverP saverI triggerO) $ SQ (config::InfluxDB) [] [] lock 
    p5 <- forkIO $ runEffect $ fromInput triggerI >-> shower
    _ <- getLine :: IO String
    mapM_ killThread [p1,p2,p3,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

data Command = Reload ST

type ST = Map Cron (Set CheckHost)

type FixmonST = StateT Monitoring IO 
type Task = (Check, Maybe Trigger)

saverP :: Database db => Input (Hostname, Maybe Trigger, [Complex]) -> Output (Hostname, Trigger) -> SaverST db ()
saverP input output = do
    runEffect $ fromInput input >-> saver >-> toOutput output
    liftIO $ performGC

cronP :: Output Task -> FixmonST ()
cronP task = do
    runEffect $ cron >-> taskMaker >-> toOutput task
    liftIO $ performGC

cron :: Producer CheckHost FixmonST ()
cron = do
    liftIO $ threadDelay $ seconds 10
    now <- liftIO getCurrentTime
    st <- _periodMap <$> get
    let tasks = unions . elems $ filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
    each tasks 
    cron

taskMaker :: Pipe CheckHost Task FixmonST ()
taskMaker = do
    Monitoring _ hosts _ triggers checks _ <- get
    CheckHost (h, c, mt) <- await
    let check = checks ! unId c
        host = hosts ! unId h
        trigger = maybe Nothing (\x -> triggers !? unId x) mt
    yield (check { chost = host }, trigger)
    taskMaker

tasker :: Pipe Task (Hostname, Maybe Trigger, [Complex]) IO ()
tasker = do
    (c, mt) <- await
    let ch = lookup (ctype c) routes
        host = chost c
    r <- liftIO $ maybe notFound (doCheck' c) ch
    yield (host, mt, r)
    tasker
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
saver = do
    timer =<< locker <$> get
    add
    saveChecks
    sendTriggers
    again =<< locker <$> get
    saver
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
shower = await >>= (\x -> liftIO $ print x) >> shower

