{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RankNTypes  #-}
module Process.Cron where

import           Types
import           Process.Configurator.Message

import           Control.Concurrent
import           Control.Distributed.Process
import           Control.Monad.State (forever, StateT, evalStateT, lift)
import qualified Control.Monad.State as S
import           Data.Binary
import           Data.Map                    (Map, elems, filterWithKey)
import           Data.Set                    (Set, unions)
import           Data.Time.Clock
import           Data.Typeable
import           System.Cron
import Control.Applicative

-- | send message every minutes to cron
clock :: Process ()
clock = do
    register "clock" =<< getSelfPid
    forever $ do
        nsend "cron" MinuteMessage
        liftIO $ threadDelay $ 10 * 1000000

type CronState a = StateT (Map Cron (Set CheckHost)) Process a

type ST = Map Cron (Set CheckHost)

data CMes = MinuteMessage
          | CronMap
          | ChangeConfig
          deriving (Typeable, Show, Eq, Enum)

instance Binary CMes where
    put MinuteMessage = put (0 :: Word8)
    put CronMap = put (1 :: Word8)
    put ChangeConfig = put (2 :: Word8)
    get = do
        a <- get :: Get Word8
        case a of
             0 -> pure MinuteMessage
             1 -> pure CronMap
             2 -> pure ChangeConfig
             _ -> error "bad binary"


-- | get message every minutes, check crontab, start checks
--  get Reload message, reload crontab

cron :: Process ()
cron = do
    say "start cron"
    register "cron" =<< getSelfPid
    say "cron (CronMap)-> configurator"
    Just x <- request CronMap
    say "cron <-(CronMap) configurator"
    evalStateT (cronT ) x

cronT :: CronState ()
cronT = forever $ do
    st <- S.get
    newSt <- lift . receiveWait $ map (\f -> f st) [messages']
    S.put newSt

messages' :: ST -> Match ST
messages' st = match fun
    where
    fun MinuteMessage = do
        now <- liftIO getCurrentTime
        let tasks = filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
        say "cron <- MinuteMessage"
        -- say $ "do tasks " ++ (show . unions . elems $ tasks)
        return st
    fun ChangeConfig = do
        say "cron <- ChangeConfig"
        Just x <- request CronMap
        return x
    fun CronMap = do
        say "cron <- CronMap"
        Just x <- request CronMap
        return x


