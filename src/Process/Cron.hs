{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Process.Cron where

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
import           Types
import Control.Applicative

-- | send message every minutes to cron
clock :: Process ()
clock = do
    register "clock" =<< getSelfPid
    forever $ do
        nsend "cron" MinuteMessage
        liftIO $ threadDelay $ 10 * 1000000

type CronState a = StateT (Map Cron (Set CheckHost)) Process a

data CMes = MinuteMessage
          | Reload (Map Cron (Set CheckHost)) deriving Typeable

instance Binary CMes where
    put MinuteMessage = put (0 :: Word8)
    put (Reload x) = put (1 :: Word8) >> put x
    get = do
        a <- get :: Get Word8
        case a of
             0 -> pure MinuteMessage
             1 -> Reload <$> get
             _ -> error "bad binary"


-- | get message every minutes, check crontab, start checks
--  get Reload message, reload crontab
cron :: Process ()
cron = do
    register "cron" =<< getSelfPid
    say "wait crontab"
    Reload x <- expect :: Process CMes
    say "load crontab"
    evalStateT cronT x

cronT :: CronState ()
cronT = forever $ do
        message <- lift $ expect :: CronState CMes
        case message of
             MinuteMessage -> do
                 now <- liftIO getCurrentTime
                 crontab <- S.get
                 let tasks = filterWithKey (\(Cron x) _ -> scheduleMatches x now) crontab
                 lift $ say $ "do tasks " ++ (show $ unions $ elems tasks)
             Reload x -> do
                 lift $ say $ "reload crontab " ++ show x
                 S.put x



