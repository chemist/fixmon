{-# LANGUAGE ScopedTypeVariables #-}
module Process.Tasker (tasker) where


import           Process.Utils
import           Types                       hiding (checks, hosts)

import           Control.Concurrent
import           Control.Distributed.Process
import           Control.Monad.State         (StateT, evalStateT, forever, lift, get, put)
import Control.Monad (forM_)
import           Data.Map                    (Map, lookup)
import           Data.Set                    (Set, toList)
import           Prelude                     hiding (lookup)

data Tasker = Tasker
  { hosts  :: Map HostId Hostname
  , checks :: Map CheckId Check
  }

type TaskerState a = StateT Tasker Process a

tasker :: Process ()
tasker = do
    register "tasker" =<< getSelfPid
    say "tasker (HostMap) -> configurator"
    Just x <- request HostMap
    say "tasker <-(HostMap) configurator"
    say "tasker (CheckMap) -> configurator"
    Just y <- request CheckMap
    say "tasker <-(CheckMap) configurator"
    evalStateT taskerT $ Tasker x y


taskerT :: TaskerState ()
taskerT = forever $ do
    st <- get
    newSt <- lift . receiveWait $ map (\f -> f st) taskerMatch
    put newSt

taskerMatch :: [Tasker -> Match Tasker]
taskerMatch = [taskMatch]

taskMatch :: Tasker -> Match Tasker
taskMatch st = match fun
    where
    fun (x :: Set CheckHost) = do
        say "tasker <- (Set CheckHost)"
        flip forM_ (startCheck st) $ toList x
        say $ show $ toList x
        return st

startCheck :: Tasker -> CheckHost -> Process ()
startCheck st ch = do
    let host'' = lookup (h ch) (hosts st)
        check'' = lookup (c ch) (checks st)
    say $ "do check: " ++ show check''
    say $ "in host: " ++ show host''
    where
        h :: CheckHost -> HostId
        h (CheckHost (x, _)) = x
        
        c :: CheckHost -> CheckId
        c (CheckHost (_, x)) = x
