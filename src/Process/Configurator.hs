module Process.Configurator
( store, readConfig )
where

import           Types
import           Process.Configurator.Message
import           Process.Configurator.Yaml
import           Process.Cron

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
    void . lift $ receiveTimeout 100000 $! map (\f -> f m) [tr, ch, pm, cch, cs]
--    lift .  say . show =<< liftIO getCurrentDirectory
    newTime <- liftIO $ getModificationTime "gnc.yaml"
    when (time /= newTime) $ do
        lift $ say "file was changed, reload"
        (_, n, _) <- liftIO readConfig
        put n
        lift $ nsend "cron" ChangeConfig
        lift $ say "configurator (ChangeConfig)-> cron  "
    storeT newTime

tr :: Monitoring -> Match ()
tr st = match fun
     where
     fun (Request (pid, TriggerId x)) = do
         say $ "configurator <- (Request Trigger)"  
         say $ "configurator (Response Trigger) -> "   
         send pid $ Response (lookup (TriggerId x) (_triggers st))

ch :: Monitoring -> Match ()
ch st = match fun
     where
     fun (Request (pid, CheckId x)) = do
         say $ "configurator <- (Request Check)"  
         say $ "configurator (Response Check) -> "   
         send pid $ Response (lookup (CheckId x) (_checks st))

pm :: Monitoring -> Match ()
pm st = match fun
     where
     fun (Request (pid, Cron x)) = do
         say $ "configurator <- (Request CronMap)"  
         say $ "configurator (Response CronMap) -> "   
         send pid $ Response (lookup (Cron x) (_periodMap st))

cch :: Monitoring -> Match ()
cch st = match fun
     where
     fun (Request (pid, CheckHost x)) = do
         say $ "configurator <- (Request CheckHosts)"  
         say $ "configurator (Response CheckHosts) -> "   
         send pid $ Response (lookup (CheckHost x) (_checkHost st))

cs :: Monitoring -> Match ()
cs st = match fun
     where
     fun (Request (pid, CronMap)) = do
         send pid $ Response (Just $ _periodMap st)
     fun _ = say "oops, bad message in cs"


