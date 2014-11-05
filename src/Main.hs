{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
module Main where

import Pipes 
import Pipes.Concurrent
import System.Cron
import Control.Monad.State
import Data.Map.Strict (Map, elems, filterWithKey, lookup)
import qualified Data.Map.Strict as M
import Data.Set (Set, unions)
import qualified Data.Set as S
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
import qualified Data.Text as T
import Prelude hiding (lookup)
import qualified Prelude 
import Debug.Trace
-- import System.Mem (performGC)

import Types ( 
               DBException, saveData, Trigger, TriggerId, countersFromExp, Dyn, Period(..)
             , Hostname(..), Complex(..), toDyn, Counter(..), Fun(..)
             , TriggerName(..), tname, Database, chost, ctype
             , getData, Env(..), Monitoring(..), CheckHost(..)
             , Cron(..), Check(..), Table(..), Status(..)
             , tdescription, tresult, unId, eval
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

tasker :: Pipe Task Triples IO ()
tasker = forever $ do
    Task c i <- await
    -- liftIO $ print c
    let ch = lookup (ctype c) routes
        host = chost c
    yield =<< liftIO (maybe (notFound i) (doCheck' host c i) ch)
    where
       notFound i = return $ CheckFail i
       doCheck' host check i doCheck'' = do
         checkResult <- try $ doCheck'' check
         case checkResult of
           Left (h :: SomeException) -> do
               print h
               return $ CheckFail i
           Right r -> return $ Triples host (Counter $ ctype check) r i

newtype SaverST a b = SST {runSST :: StateT (SaveQueue a) IO b}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (SaveQueue a))

data SaveQueue a = SQ
  { database :: a
  , queueCheck :: [(Hostname, Counter, [Complex])]
  }

data Triples = Triples Hostname Counter [Complex] CheckHost
             | STriples Hostname Counter Complex CheckHost
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
               Triples h prefixCounter c ch -> do
                   modify $ \x -> x { queueCheck = (h, prefixCounter, c) : queueCheck x }
                   each $ map (\x -> STriples h prefixCounter x ch) c
                   add 
               STriples{} -> yield triples >> add
               CheckFail{} -> yield triples >> add
               Dump -> return ()



shower :: (Show a, MonadIO m) => Consumer a m ()
shower = forever $ await >>= liftIO . print

checkTrigger :: Pipe Triples (Hostname, Complex) FixmonST ()
checkTrigger = forever $ do
    _db <- storage <$> get
    work =<< await
    -- r <- liftIO $ eval (Env (getData db (Table h))) (tresult t)
    -- saveTriggers [(Hostname h, [triggerToComplex t r])]
    -- yield (Hostname h, triggerToComplex t r)
    -- liftIO $ print $ show h <> " " <>  show c <> " " <> show i
    -- where
      -- saveTriggers queue = liftIO $ saveData db queue `catch` \(e :: DBException) -> print e
work :: Triples -> Pipe Triples (Hostname, Complex) FixmonST ()
work (STriples (Hostname h) prefixCounter c i) = do
    tm <- triggerMap <$> get
    tv <- triggers <$> get
    let trsm = lookup i tm
    when (isJust trsm) $ do  -- if not in triggerMap skip, and wait next
        let trs = fromJust trsm
            -- TODO: can be evaluated when application start
            keys = S.fold fun S.empty $ S.map (\x -> countersFromExp $ tresult $ tv ! unId x) trs
            fun x y = S.unions (y : Prelude.map (fst . unSplitCounter) x)
            ikeys = mapMaybe unSplitId $ Prelude.concat $ S.toList $ S.map (\x -> countersFromExp $ tresult $ tv ! unId x) trs
        when (isRightComplex c ikeys) $ do
            liftIO $ print prefixCounter
            liftIO $ print $ S.map (\x -> countersFromExp $ tresult $ tv ! unId x) trs
            liftIO $ print c
            -- liftIO $ print $ removeUnusedFromComplex keys c
            yield (Hostname h, removeUnusedFromComplex keys c)
--     mapM_ work' triggersToDo
work _ = undefined

type Cash = Map (Hostname, Counter) Dyn

createEnv :: Database db => db -> Table -> Cash -> Env
createEnv db t cash = Env (createEnv' db t cash)
  where
  createEnv' db' (Table hostname) cash' (EnvValFun c) = 
    let v = M.lookup (Hostname hostname, c) cash'
    in case v of
            Nothing -> createEnv' db' (Table hostname) cash' (LastFun c (Count 1))
            Just r -> return r
  createEnv' db' t' _ f' = getData db' t' f'

isRightComplex :: Complex -> [Dyn] -> Bool
isRightComplex (Complex xs) ids =
    let id' = Prelude.lookup "id" xs
    in case id' of
            Nothing -> False
            Just id'' -> elem id'' ids

unSplitId :: Counter -> Maybe Dyn
unSplitId (Counter x) =
    case T.splitOn ":" x of
         [_] -> Nothing
         [d,_] -> Just (toDyn d)
         _ -> error "some thing wrong"

unSplitCounter :: Counter -> (S.Set Counter, Maybe Dyn)
unSplitCounter (Counter x) = 
    case T.splitOn ":" x of
       [a] -> (S.singleton $ Counter a, Nothing)
       [d,b] -> (S.fromList [Counter b, Counter $ T.dropWhileEnd (/= '.') b <> "id"], Just $ toDyn d)
       _ -> error "something wrong"

removeUnusedFromComplex :: S.Set Counter -> Complex -> Complex
removeUnusedFromComplex keys (Complex xs) = Complex $ filter (\(x,_) -> S.member x keys) xs


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


