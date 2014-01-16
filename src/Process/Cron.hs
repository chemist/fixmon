{-# LANGUAGE OverloadedStrings #-}
module Process.Cron where

import           Control.Concurrent
import           Control.Distributed.Process
import           Control.Monad.State
import           Data.Map                    (Map, elems, filterWithKey)
import           Data.Set                    (Set, unions)
import           Data.Time.Clock
import           Types
import           GNS.Message
import           System.Cron

-- | send message every minutes to cron
clock :: Process ()
clock = do
    register "clock" =<< getSelfPid
    forever $ do
        nsend "cron" MinuteMessage
        liftIO $ threadDelay $ 10 * 1000000

type CronState a = StateT (Map Cron (Set CheckHost)) Process a

-- | get message every minutes, check crontab, start checks
--  get Reload message, reload crontab
cron :: Monitoring -> Process ()
cron m = do
    register "cron" =<< getSelfPid
    evalStateT cronT (_periodMap m) 

cronT :: CronState ()
cronT = forever $ do
        message <- lift $ expect :: CronState CMes
        case message of
             MinuteMessage -> do
                 now <- liftIO getCurrentTime
                 crontab <- get
                 let tasks = filterWithKey (\(Cron x) _ -> scheduleMatches x now) crontab
                 lift $ say $ "do tasks " ++ (show $ unions $ elems tasks)
             Reload x -> do
                 lift $ say $ "reload crontab " ++ show x
                 put x


