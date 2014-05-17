{-# LANGUAGE ScopedTypeVariables #-}
module Process.Tasker (tasker) where


import           Process.Utils
import           Types

import           Control.Distributed.Process
import           Control.Monad.State         (StateT, evalStateT, forever, get,
                                              lift, put)
import           Data.Set                    (Set, toList)
import           Data.Vector                 (Vector, (!))
import           Prelude                     hiding (lookup)

data Tasker = Tasker
  { hosts  :: Vector Hostname
  , checks :: Vector Check
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
        mapM_ (startCheck st) $ toList x
        say $ show $ toList x
        return st

startCheck :: Tasker -> CheckHost -> Process ()
startCheck st ch = do
    let host'' = hosts st ! h ch
        check'' = checks st ! c ch
    say $ "do check: " ++ show check''
    say $ "in host: " ++ show host''
    where
        h :: CheckHost -> Int
        h (CheckHost (i, _)) = unId i

        c :: CheckHost -> Int
        c (CheckHost (_, i)) = unId i
