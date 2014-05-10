module Process.Configurator
( store)
where

import           Process.Configurator.Yaml
import           Process.Utils
import           Types

import           Control.Distributed.Process
import           Control.Monad.State
import           Data.Map                    (lookup)
import           Data.Time                   (UTCTime)
import           Data.Vector                 ((!?))
import           Prelude                     hiding (lookup)
import           System.Directory

type StoreT a = StateT Monitoring Process a

store :: Process ()
store = do
    say "start configurator"
    register "configurator" =<< getSelfPid
    Right m <- liftIO $ parseConfig "gnc.yaml"
--    say . show =<< liftIO getCurrentDirectory
    time <- liftIO $ getModificationTime "gnc.yaml"
    evalStateT (storeT time) m

storeT :: UTCTime -> StoreT ()
storeT time = do
    m <- get
    void . lift $ receiveWait $! map (\f -> f m) storeMatch
--    lift .  say . show =<< liftIO getCurrentDirectory
    newTime <- liftIO $ getModificationTime "gnc.yaml"
    when (time /= newTime) $ do
        lift $ say "file was changed, reload"
        Right n <- liftIO $ parseConfig "gnc.yaml"
        put n
        lift $ nsend "cron" CronMap
        lift $ say "configurator (CronMap)-> cron  "
        lift $ nsend "tasker" HostMap
        lift $ say "configurator (HostMap)-> tasker  "
        lift $ nsend "tasker" CheckMap
        lift $ say "configurator (CheckMap)-> tasker  "
    storeT newTime

storeMatch :: [Monitoring -> Match ()]
storeMatch = [ lookupTrigger
             , lookupCheck
             , lookupCronSet
             , lookupCheckHost
             , getPart
             ]

lookupTrigger :: Monitoring -> Match ()
lookupTrigger st = match fun
     where
     fun (Request (pid, TriggerId x)) = do
         say "configurator <- (Request Trigger)"
         say "configurator (Response Trigger) -> "
         send pid $ Response ((_triggers st) !? x)

lookupCheck :: Monitoring -> Match ()
lookupCheck st = match fun
     where
     fun (Request (pid, CheckId x)) = do
         say "configurator <- (Request Check)"
         say "configurator (Response Check) -> "
         send pid $ Response ((_checks st) !? x)

lookupCronSet :: Monitoring -> Match ()
lookupCronSet st = match fun
     where
     fun (Request (pid, Cron x)) = do
         say "configurator <- (Request CronSet)"
         say "configurator (Response CronSet) -> "
         send pid $ Response (lookup (Cron x) (_periodMap st))

lookupCheckHost :: Monitoring -> Match ()
lookupCheckHost st = match fun
     where
     fun (Request (pid, CheckHost x)) = do
         say "configurator <- (Request CheckHosts)"
         say "configurator (Response CheckHosts) -> "
         send pid $ Response (lookup (CheckHost x) (_checkHost st))

getPart :: Monitoring -> Match ()
getPart st = match fun
     where
     fun (Request (pid, CronMap)) = do
         say "configurator <- (Request CronMap)"
         say "configurator (Response CronMap) -> "
         send pid $ Response (Just $ _periodMap st)
     fun (Request (pid, HostMap)) = do
         say "configurator <- (Request HostMap)"
         say "configurator (Response HostMap) -> "
         send pid $ Response (Just $ _hosts st)
     fun (Request (pid, CheckMap)) = do
         say "configurator <- (Request CheckMap)"
         say "configurator (Response CheckMap) -> "
         send pid $ Response (Just $ _checks st)
     fun (Request (_, _)) = do
         say "configurator <- (BadMessage)"

