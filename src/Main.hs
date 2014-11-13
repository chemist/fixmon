{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes   #-}
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
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Prelude 
-- import Debug.Trace
-- import System.Mem (performGC)

import Types ( 
               DBException(..), saveData, Trigger(..), TriggerId, countersFromExp, Dyn, Period(..)
             , Hostname(..), Complex(..), Convert(..), Counter(..), Fun(..), HostId, TriggerHost(..)
             , Database, chost, ctype
             , getData, Env(..), Monitoring(..), CheckHost(..)
             , Cron(..), Check(..), Table(..), Status(..)
             , eval
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
    p5 <- forkIO $ run m (fromInput triggerI >-> checkTrigger >-> shower) M.empty
    _ <- getLine :: IO String
    mapM_ killThread [p0,p1,p2,p3,p4,p5]

seconds :: Int -> Int
seconds = (* 1000000)

type Cache = Map (Hostname, Counter) Dyn

data Task = Task Check CheckHost

newtype Fixmon a b = Fixmon { runFixmon :: ReaderT Monitoring (StateT a IO) b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState a, MonadReader Monitoring)

run :: forall s a. Monitoring -> Effect (Fixmon s) a -> s -> IO a
run m = evalStateT . (flip runReaderT m) . runFixmon . runEffect

type SaveQueue = [(Hostname, Prefix, [Complex])]

type Prefix = Counter -- ctype in check

data Triples = Triples Prefix Complex CheckHost
             | CheckFail CheckHost SomeException
             | Dump


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
    Monitoring _ hosts' _ _ checks' _ snmp' _ <- ask
    CheckHost (h, c) <- await
    let check = checks' ! from c
        host = hosts' ! from h
    yield $ Task (check { chost = host, csnmp = Just $ fromMaybe snmp' (csnmp check)}) (CheckHost (h,c))

tasker :: Pipe Task Triples IO ()
tasker = forever $ do
    Task c i <- await
    -- liftIO $ print c
    let ch = lookup (ctype c) routes
    each =<< liftIO (maybe (notFound i) (doCheck' c i) ch)
    where
       notFound i = return $ [CheckFail i (SomeException EmptyException)]
       doCheck' check i doCheck'' = do
         checkResult <- try $ doCheck'' check
         case checkResult of
           Left (h :: SomeException) -> do
               print h
               return $ [CheckFail i h]
           Right r -> return $ map (\x -> Triples (Counter $ ctype check) x i) r

saver :: Pipe Triples Triples (Fixmon [(Hostname, Prefix, Complex)]) ()
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
                   yield triples
                   add 
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

checkTrigger :: Pipe Triples (TriggerHost, Status) (Fixmon Cache) ()
checkTrigger = forever $ do
    work =<< await
    -- r <- liftIO $ eval (Env (getData db (Table h))) (tresult t)
    -- saveTriggers [(Hostname h, [triggerToComplex t r])]
    -- yield (Hostname h, triggerToComplex t r)
    -- liftIO $ print $ show h <> " " <>  show c <> " " <> show i
    -- where
      -- saveTriggers queue = liftIO $ saveData db queue `catch` \(e :: DBException) -> print e
work :: Triples -> Pipe Triples (TriggerHost, Status) (Fixmon Cache) ()
work (CheckFail i e) = do
    tm <- triggerMap <$> ask
    let trsm = lookup i tm
        CheckHost (hid, _) = i
        til = S.toList $ fromJust trsm
        result = repeat (Left e)
    when (isJust trsm) $ do
        each $ toTriggerHost hid til result
work (Triples prefixCounter c i) = do
    tm <- triggerMap <$> ask
    h <- getHost i 
    let trsm = lookup i tm
        CheckHost (hid, _) = i
    when (isJust trsm) $ do  -- if not in triggerMap skip, and wait next
            -- TODO: can be evaluated when application start
        ikeys <- getCountersById (fromJust trsm)
        when (isCombined c $ M.keysSet ikeys) $ do
            let toCache = map (\(x,y) -> ((h, x), y)) $ removeUnusedFromCombined ikeys prefixCounter c
                til = S.toList $ fromJust trsm
            modify (M.union (M.fromList toCache))
            cache <- get
            db <- storage <$> ask
            tr <- Prelude.mapM getTrigger til
            liftIO $ print tr
            result <- liftIO $ mapM (\x -> eval (createEnv db (Table $ from h) cache ) (tresult x)) tr
            each $ toTriggerHost hid til result 
work _ = undefined

type IKeys = Map Dyn (Set Counter)

toTriggerHost :: HostId -> [TriggerId] -> [Either SomeException Bool] -> [(TriggerHost, Status)]
toTriggerHost hid tids rs = map (\(t, r) -> (TriggerHost (hid, t), toStatus r)) $  zip tids rs

toStatus :: Either SomeException Bool -> Status
toStatus (Right True) = Ok
toStatus (Right False) = Bad "false"
toStatus (Left e) = Bad $ show e

createEnv :: Database db => db -> Table -> Cache -> Env
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

removeUnusedFromCombined :: IKeys -> Prefix -> Complex -> [(Counter, Dyn)]
removeUnusedFromCombined keys (Counter pr) (Complex xs) =
    let id' = fromJust $ Prelude.lookup (Counter "id") xs
        ikeys x = fromJust $ M.lookup x keys
        fun (Counter x,y) = (Counter (from id' <> ":" <> pr <> "." <> x), y)
    in map fun $ Prelude.filter (\(x,_) -> S.member x (ikeys id')) xs

