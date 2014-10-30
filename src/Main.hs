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
import Data.Vector ((!))
import System.IO 
import Control.Exception
import Data.Monoid ((<>))
import Data.Maybe
import Data.Text (pack)
import Prelude hiding (lookup)
import Debug.Trace
-- import System.Mem (performGC)

import Types ( 
               DBException, saveData, Trigger
             , Hostname(..), Complex(..), toDyn, Counter(..) 
             , TriggerName(..), tname, Database, chost, ctype
             , getData, Env(..), Monitoring(..), CheckHost(..)
             , Cron(..), Check(..), Table(..), Status(..)
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
                  $ SQ (storage m) [] 
    p5 <- forkIO $ (evalStateT . runEffect)
                  (fromInput triggerI >-> checkTrigger >-> shower)
                  m
    _ <- getLine :: IO String
    mapM_ killThread [p0,p1,p2,p3,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

data Command = Reload ST

type ST = Map Cron (Set CheckHost)

type FixmonST = StateT Monitoring IO 

data Task = Task Check CheckHost

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
    Monitoring _ hosts' _ _ checks' _ _ snmp' _ <- get
    CheckHost (h, c) <- await
    let check = checks' ! unId c
        host = hosts' ! unId h
    yield $ Task (check { chost = host, csnmp = Just $ fromMaybe snmp' (csnmp check)}) (CheckHost (h,c))

checkTrigger :: Pipe Triples (Hostname, Complex) FixmonST ()
checkTrigger = forever $ do
    _db <- storage <$> get
    Triples (Hostname h) c i <- await
    -- r <- liftIO $ eval (Env (getData db (Table h))) (tresult t)
    -- saveTriggers [(Hostname h, [triggerToComplex t r])]
    -- yield (Hostname h, triggerToComplex t r)
    liftIO $ print $ show h <> " " <>  show c <> " " <> show i
    yield (Hostname h, Complex [])
    -- where
      -- saveTriggers queue = liftIO $ saveData db queue `catch` \(e :: DBException) -> print e

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
    Task c i <- await
    -- liftIO $ print c
    let ch = lookup (ctype c) routes
        host = chost c
    yield =<< (liftIO $ maybe (notFound i) (doCheck' host c i) ch)
    where
       notFound i = return $ CheckFail i
       doCheck' host check i doCheck'' = do
         checkResult <- try $ doCheck'' check
         case checkResult of
           Left (h :: SomeException) -> do
               print h
               return $ CheckFail i
           Right r -> return $ Triples host r i

newtype SaverST a b = SST {runSST :: StateT (SaveQueue a) IO b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (SaveQueue a))

data SaveQueue a = SQ
  { database :: a
  , queueCheck :: [(Hostname, [Complex])]
  }

data Triples = Triples Hostname [Complex] CheckHost
             | CheckFail CheckHost
             | Dump
             deriving (Show)



saver :: Database db => Pipe Triples Triples (SaverST db) ()
saver = forever $ do
    add
    saveChecks
    again 
    where
      saveChecks = do
          ch <- queueCheck <$> get
          db <- database <$> get 
          liftIO $ saveData db ch `catch` (\(e :: DBException) -> print e)
      again = do
          db <- database <$> get
          put $ SQ db [] 
      add = do
          triples <- await
          case triples of
               Triples h c _ -> do
                   modify $ \x -> x { queueCheck = (h,c) : queueCheck x }
                   yield triples
                   add 
               CheckFail _ -> yield triples
               Dump -> return ()



shower :: (Show a, MonadIO m) => Consumer a m ()
shower = forever $ await >>= (\x -> liftIO $ print x)


type SwitchFunc state event = state -> event -> state

newtype FSM state a = FSM ( state -> (state, a) )

