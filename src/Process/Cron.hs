{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Cron (clock, cron) where

import           Process.Utils
import           Types

import           Control.Concurrent
import           Control.Distributed.Process
import           Control.Monad.State         (StateT, evalStateT, forever, lift)
import qualified Control.Monad.State         as S
import           Data.Map                    (Map, elems, filterWithKey)
import           Data.Set                    (Set, unions)
import           Data.Time.Clock
import           System.Cron

-- | send message every minutes to cron
clock :: Process ()
clock = do
    register "clock" =<< getSelfPid
    forever $ do
        nsend "cron" MinuteMessage
        liftIO $ threadDelay $ 10 * 1000000

type ST = Map Cron (Set CheckHost)
type CronState a = StateT ST Process a

-- | get message every minutes, check crontab, start checks
--  get Reload message, reload crontab
cron :: Process ()
cron = do
    say "start cron"
    register "cron" =<< getSelfPid
    say "cron (CronMap)-> configurator"
    Just x <- request CronMap
    say "cron <-(CronMap) configurator"
    evalStateT cronT x

cronT :: CronState ()
cronT = forever $ do
    st <- S.get
    newSt <- lift . receiveWait $ map (\f -> f st) cronMatch
    S.put newSt

cronMatch :: [ST -> Match ST]
cronMatch = [cronMatch']

cronMatch' :: ST -> Match ST
cronMatch' st = match fun
    where
    fun MinuteMessage = do
        now <- liftIO getCurrentTime
        let tasks = filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
        say "cron <- (MinuteMessage)"
        nsend "tasker" (unions . elems $ tasks)
        say "cron (Set CheckHost) -> tasker"
        -- say $ "do tasks " ++ (show . unions . elems $ tasks)
        return st
    fun CronMap = do
        say "cron <- CronMap"
        Just x <- request CronMap
        return x
    fun _ = do
        say "cron <- BadMessage"
        return st


