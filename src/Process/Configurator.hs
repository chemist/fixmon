module Process.Configurator
( store, readConfig )
where

import           Types
import           Process.Utils
import           Process.Configurator.Yaml

import           Control.Distributed.Process
import           Control.Monad.State
import System.Directory
import Data.Time (UTCTime)
import           Data.Map                     (lookup)
import           Prelude                      hiding (lookup)

readConfig :: IO (Either String (), Monitoring, Log)
readConfig = runGns (StartOptions "gnc.yaml") emptyMonitoring parseConfig

type StoreT a = StateT Monitoring Process a

store :: Process ()
store = do
    say "start configurator"
    register "configurator" =<< getSelfPid
    (_, m, _) <- liftIO readConfig
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
        (_, n, _) <- liftIO readConfig
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
         send pid $ Response (lookup (TriggerId x) (_triggers st))

lookupCheck :: Monitoring -> Match ()
lookupCheck st = match fun
     where
     fun (Request (pid, CheckId x)) = do
         say "configurator <- (Request Check)"  
         say "configurator (Response Check) -> "   
         send pid $ Response (lookup (CheckId x) (_checks st))

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

