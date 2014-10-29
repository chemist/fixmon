{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
module Main where

import Pipes 
import Pipes.Concurrent
import System.Cron
import Control.Monad.State
import Data.Map.Strict (Map, elems, filterWithKey, lookup)
import Data.Set (Set, unions)
import Data.Time.Clock
import Control.Concurrent (threadDelay, killThread)
import Process.Configurator.Yaml
import Process.Web
import Control.Applicative
import Data.Vector ((!?), (!))
import System.IO 
import Control.Exception
import Data.Monoid ((<>))
import Data.Maybe
import Data.Text (pack)
import Network.Protocol.Snmp (ClientException(..))
import Prelude hiding (lookup)
-- import System.Mem (performGC)

import Types ( 
               DBException, saveData, Trigger
             , Hostname(..), Complex(..), toDyn, Counter(..) 
             , TriggerName(..), tname, Database, chost, ctype
             , getData, Env(..), Monitoring(..), CheckHost(..)
             , Cron(..), Check(..), Table(..)
             , tdescription, tresult, unId, eval
             )
import Checks (routes)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Right m <- parseConfig "fixmon.yaml"
    (taskO, taskI) <- spawn Unbounded
    (saverO, saverI) <- spawn Unbounded
    (triggerO, triggerI) <- spawn Unbounded
    p0 <- forkIO $ web
    p1 <- forkIO $ (evalStateT . runEffect)  
                  (cron >-> taskMaker >-> toOutput taskO) 
                  m
    p2 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
--    p3 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    p3 <- forkIO $ runEffect $ dumpMessages >-> toOutput saverO
    p4 <- forkIO $ (evalStateT . runSST . runEffect ) 
                  (fromInput saverI >-> saver >-> toOutput triggerO) 
                  $ SQ (storage m) [] [] 
    p5 <- forkIO $ (evalStateT . runEffect)
                  (fromInput triggerI >-> checkTrigger (storage m) >-> shower)
                  m
    _ <- getLine :: IO String
    mapM_ killThread [p0,p1,p2,p3,p4,p5]

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
    st <- periodMap <$> get
    let tasks = unions . elems $ filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
    each tasks 

dumpMessages :: Producer Triples IO ()
dumpMessages = forever $ do
    liftIO $ threadDelay $ seconds 1
    yield Dump

taskMaker :: Pipe CheckHost Task FixmonST ()
taskMaker = forever $ do
    Monitoring _ hosts' _ triggers' checks' _ snmp' _ <- get
    CheckHost (h, c, mt) <- await
    let check = checks' ! unId c
        host = hosts' ! unId h
        trigger = maybe Nothing (\x -> triggers' !? unId x) mt
    yield (check { chost = host, csnmp = Just $ fromMaybe snmp' (csnmp check)}, trigger)

checkTrigger :: Database db => db -> Pipe (Hostname, Trigger) (Hostname, Complex) FixmonST ()
checkTrigger db = forever $ do
    (Hostname h,t) <- await
    r <- liftIO $ eval (Env (getData db (Table h))) (tresult t)
    saveTriggers [(Hostname h, [triggerToComplex t r])]
    yield (Hostname h, triggerToComplex t r)
    where
      saveTriggers queue = liftIO $ saveData db queue `catch` \(e :: DBException) -> print e

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
 

tasker :: Pipe Task Triples IO ()
tasker = forever $ do
    (c, mt) <- await
    liftIO $ print c
    let ch = lookup (ctype c) routes
        host = chost c
    r <- liftIO $ maybe (notFound c) (doCheck' c) ch
    yield $ Triples host mt r
    where
       notFound check = return [Complex [(Counter $ (ctype check) <> "._status_", toDyn False)]]
       doCheck' check doCheck'' = do
         checkResult <- try $ doCheck'' check
         case checkResult of
           Left (h :: SomeException) -> do
               print h
               return [Complex [(Counter $ (ctype check) <> "._status_", toDyn False)]]
--           Left (h :: ClientException) -> do
--               print h
--               return [Complex [(Counter $ (ctype check) <> "._status_", toDyn False)]]
           Right r -> return r

newtype SaverST a b = SST {runSST :: StateT (SaveQueue a) IO b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (SaveQueue a))

data SaveQueue a = SQ
  { database :: a
  , queueCheck :: [(Hostname, [Complex])]
  , queueTrigger :: [(Hostname, Trigger)]
  }

data Triples = Triples Hostname (Maybe Trigger) [Complex]
             | Dump
             deriving (Show)



saver :: Database db => Pipe Triples (Hostname, Trigger) (SaverST db) ()
saver = forever $ do
    add
    saveChecks
    sendTriggers
    again 
    where
      saveChecks = do
          ch <- queueCheck <$> get
          db <- database <$> get 
          liftIO $ saveData db ch `catch` (\(e :: DBException) -> print e)
      sendTriggers = do
          t <- queueTrigger <$> get
          each t
      again = do
          db <- database <$> get
          put $ SQ db [] [] 
      add = do
          triples <- await
          case triples of
               Triples h mt c -> do
                   modify $ \x -> x { queueCheck = (h,c) : queueCheck x }
                   when (isJust mt) $ modify $ \x -> x { queueTrigger = (h,fromJust mt) : queueTrigger x }
                   add 
               Dump -> return ()



shower :: (Show a, MonadIO m) => Consumer a m ()
shower = forever $ await >>= (\x -> liftIO $ print x)

