{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
module Main where

import Pipes 
import Pipes.Concurrent
import System.Cron
import Control.Monad.State
import Control.Monad.Reader
import Data.Map.Strict (Map, elems, filterWithKey, lookup)
import qualified Data.Map.Strict as M
import Data.Set (Set, unions)
import qualified Data.Set as S
import Data.Time.Clock
import Control.Concurrent (threadDelay, killThread)
import Configurator.Yaml
import Web
import Control.Applicative
import Data.Vector ((!))
import System.IO 
import Control.Exception
import Data.Monoid ((<>))
import Data.Maybe
import Data.Text (pack)
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Prelude 
-- import Debug.Trace
-- import System.Mem (performGC)

import Types ( 
               DBException, saveData, Trigger, TriggerId, countersFromExp, Dyn, Period(..)
             , Hostname(..), Complex(..), Convert(..), Counter(..), Fun(..)
             , TriggerName(..), tname, Database, chost, ctype
             , getData, Env(..), Monitoring(..), CheckHost(..)
             , Cron(..), Check(..), Table(..), Status(..)
             , tdescription, tresult, eval
             )
import Checks (routes)

-- PEN assigned for fixmon
-- Prefix: iso.org.dod.internet.private.enterprise (1.3.6.1.4.1)
-- PEN: 44729

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Right m <- parseConfig "fixmon.yaml"
    (taskO, taskI) <- spawn Unbounded
    (saverO, saverI) <- spawn Unbounded
    (triggerO, triggerI) <- spawn Unbounded
    p0 <- forkIO web
    p1 <- forkIO $ run m (cron >-> taskMaker >-> toOutput taskO) ()
    p2 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
--    p3 <- forkIO $ runEffect $ fromInput taskI >-> tasker >-> toOutput saverO
    p3 <- forkIO $ runEffect $ dumpMessages >-> toOutput saverO
    p4 <- forkIO $ run m (fromInput saverI >-> saver >-> toOutput triggerO) [] 
    p5 <- forkIO $ run m (fromInput triggerI >-> checkTrigger >-> shower) ()
    _ <- getLine :: IO String
    mapM_ killThread [p0,p1,p2,p3,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

type Cash = Map (Hostname, Counter) Dyn

data Task = Task Check CheckHost

newtype Fixmon a b = Fixmon { runFixmon :: ReaderT Monitoring (StateT a IO) b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState a, MonadReader Monitoring)

run :: forall s a. Monitoring -> Effect (Fixmon s) a -> s -> IO a
run m = evalStateT . (flip runReaderT m) . runFixmon . runEffect

type SaveQueue = [(Hostname, Counter, [Complex])]

type Prefix = Counter -- ctype in check

data Triples = Triples Prefix [Complex] CheckHost
             | STriples Prefix Complex CheckHost
             | CheckFail CheckHost
             | Dump
             deriving (Show)


cron :: Producer CheckHost (Fixmon ()) ()
cron = forever $ do
    liftIO $ threadDelay $ seconds 10
    now <- liftIO getCurrentTime
    st <- periodMap <$> ask
    let tasks = unions . elems $ filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
    each tasks 

dumpMessages :: Producer Triples IO ()
dumpMessages = forever $ do
    liftIO $ threadDelay $ seconds 1
    yield Dump

taskMaker :: Pipe CheckHost Task (Fixmon ()) ()
taskMaker = forever $ do
    Monitoring _ hosts' _ _ checks' _ _ snmp' _ <- ask
    CheckHost (h, c) <- await
    let check = checks' ! from c
        host = hosts' ! from h
    yield $ Task (check { chost = host, csnmp = Just $ fromMaybe snmp' (csnmp check)}) (CheckHost (h,c))

tasker :: Pipe Task Triples IO ()
tasker = forever $ do
    Task c i <- await
    -- liftIO $ print c
    let ch = lookup (ctype c) routes
    yield =<< liftIO (maybe (notFound i) (doCheck' c i) ch)
    where
       notFound i = return $ CheckFail i
       doCheck' check i doCheck'' = do
         checkResult <- try $ doCheck'' check
         case checkResult of
           Left (h :: SomeException) -> do
               print h
               return $ CheckFail i
           Right r -> return $ Triples (Counter $ ctype check) r i

saver :: Pipe Triples Triples (Fixmon SaveQueue) ()
saver = forever $ do
    add
    saveChecks
    put []
    where
      saveChecks = do
          queue <- get
          db <- storage <$> ask 
          liftIO $ saveData db queue `catch` (\(e :: DBException) -> print e)
      add = do
          triples <- await
          case triples of
               Triples prefixCounter c ch -> do
                   h <- getHost ch 
                   modify $ (:) (h, prefixCounter, c)
                   each $ map (\x -> STriples prefixCounter x ch) c
                   add 
               STriples{} -> error "saver, Striples"
               CheckFail{} -> yield triples >> add
               Dump -> return ()

getHost :: (Functor m, Monad m, MonadReader Monitoring m)  => CheckHost -> m Hostname
getHost (CheckHost (i, _)) = (\x -> hosts x ! from i) <$> ask 

getTrigger :: (Functor m, Monad m, MonadReader Monitoring m) => TriggerId -> m Trigger
getTrigger i = (\x -> triggers x ! from i) <$> ask

getCountersById :: (Functor m, Monad m, MonadReader Monitoring m) => Set TriggerId -> m (Map Dyn (Set Counter))
getCountersById sti = do
    tv <- triggers <$> ask
    let a = Prelude.concat $ S.toList $ S.map (\x -> countersFromExp $ tresult $ tv ! from x) sti
        counterToIdValue (Counter x) = case T.splitOn ":" x of
                                            [y] -> ((to T.empty), (S.singleton $ stripBaseInCounter $ Counter y))
                                            [d,y] -> ((to d), (S.singleton $ stripBaseInCounter $ Counter y))
                                            _ -> error "counteToIdValue"
        stripBaseInCounter (Counter x) = Counter . snd $ T.breakOnEnd "." x
    return $ M.fromListWith S.union $ map counterToIdValue a

shower :: (Show a, MonadIO m) => Consumer a m ()
shower = forever $ await >>= liftIO . print

checkTrigger :: Pipe Triples (Hostname, Complex) (Fixmon ()) ()
checkTrigger = forever $ do
    work =<< await
    -- r <- liftIO $ eval (Env (getData db (Table h))) (tresult t)
    -- saveTriggers [(Hostname h, [triggerToComplex t r])]
    -- yield (Hostname h, triggerToComplex t r)
    -- liftIO $ print $ show h <> " " <>  show c <> " " <> show i
    -- where
      -- saveTriggers queue = liftIO $ saveData db queue `catch` \(e :: DBException) -> print e
work :: Triples -> Pipe Triples (Hostname, Complex) (Fixmon ()) ()
work (CheckFail i) = do
    tm <- triggerMap <$> ask
    let trsm = lookup i tm
    h <- getHost i 
    when (isJust trsm) $ do
        let trs = fromJust trsm
        liftIO $ print $ "trigger fail " ++ show trs
        yield (h, Complex [])
work (STriples prefixCounter c i) = do
    tm <- triggerMap <$> ask
    h <- getHost i 
    let trsm = lookup i tm
    when (isJust trsm) $ do  -- if not in triggerMap skip, and wait next
            -- TODO: can be evaluated when application start
        ikeys <- getCountersById (fromJust trsm)
        when (isCombined c $ M.keysSet ikeys) $ do
            liftIO $ print prefixCounter
            liftIO $ print ikeys
            liftIO $ print c
            r <- Prelude.mapM getTrigger (S.toList $ fromJust trsm)
            liftIO $ print r
            liftIO $ print ("" :: String)
            yield (h, removeUnusedFromCombined ikeys c)
--     mapM_ work' triggersToDo
work _ = undefined


createEnv :: Database db => db -> Table -> Cash -> Env
createEnv db t cash = Env (createEnv' t)
  where
  createEnv' (Table hostname) (EnvValFun c) = 
    let v = M.lookup (Hostname hostname, c) cash
    in case v of
            Nothing -> createEnv' t (LastFun c (Count 1))
            Just r -> return r
  createEnv' _ f  = getData db t f

-- combined check when Check -> [Complex]
isCombined :: Complex -> Set Dyn -> Bool
isCombined (Complex xs) ids =
    let id' = Prelude.lookup "id" xs
    in case id' of
            Nothing -> False
            Just id'' -> S.member id'' ids

removeUnusedFromCombined :: Map Dyn (Set Counter) -> Complex -> Complex
removeUnusedFromCombined keys (Complex xs) =
    let id' = fromJust $ Prelude.lookup (Counter "id") xs
        ikeys x = fromJust $ M.lookup x keys
    in Complex $ Prelude.filter (\(x,_) -> S.member x (ikeys id')) xs


data CounterType = State
                 | Message

triggerToComplex :: Trigger -> Either DBException Bool -> Complex
triggerToComplex tr (Left e) = Complex [ (toCounter tr State , to False)
                                       , (toCounter tr Message, to $ pack $ show e)
                                       ]
triggerToComplex tr (Right True) = Complex [ (toCounter tr State, to True) ]
triggerToComplex tr (Right False) = Complex [ (toCounter tr State, to False)
                                            , (toCounter tr Message, to $ tdescription tr)
                                            ]

toCounter :: Trigger -> CounterType -> Counter
toCounter tr State = let TriggerName tn = tname tr
                     in Counter $ "trigger." <> tn <> ".status"
toCounter tr Message = let TriggerName tn = tname tr
                       in Counter $ "trigger." <> tn <> ".message"


