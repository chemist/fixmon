module Main where

import Pipes 
import Pipes.Concurrent
import System.Cron
import Types
import Control.Monad.State
import Data.Map.Strict (Map, elems, filterWithKey)
import Data.Set (Set, unions)
import Data.Time.Clock
import Control.Concurrent (threadDelay, killThread)
import Process.Configurator.Yaml
import Control.Applicative
import Data.Vector ((!?), (!))
import System.IO 
import Data.Maybe
-- import System.Mem (performGC)



main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    Right m <- parseConfig "fixmon.yaml"
    (taskO, taskI) <- spawn Unbounded
    p1 <- forkIO $ evalStateT (cronP taskO) m
    p2 <- forkIO $ runEffect $ fromInput taskI >-> shower1
    p3 <- forkIO $ runEffect $ fromInput taskI >-> shower2
    _ <- getLine :: IO String
    mapM_ killThread [p1,p2,p3]

seconds :: Int -> Int
seconds = (* 1000000)

data Command = Reload ST

type ST = Map Cron (Set CheckHost)

type FixmonST = StateT Monitoring IO 
type Task = (Check, Maybe Trigger)

cronP :: Output Task -> FixmonST ()
cronP task = do
    runEffect $ cron >-> taskMaker >-> toOutput task
    liftIO $ performGC

cron :: Producer CheckHost FixmonST ()
cron = do
    liftIO $ print "new iterate"
    liftIO $ threadDelay $ seconds 5
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

tasker :: Pipe Task (Hostname, Maybe Trigger, Complex) IO ()
tasker = do
    (c, mt) <- await
    -- let ch = lookup (ctype c) routes
    undefined


shower2 :: (Show a, MonadIO m) => Consumer a m ()
shower2 = await >>= (\x -> liftIO $ print $ "second" ++ show x) >> shower2

shower1 :: (Show a, MonadIO m) => Consumer a m ()
shower1 = await >>= (\x -> liftIO $ print $ "first" ++ show x) >> shower1

