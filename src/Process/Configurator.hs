module Process.Configurator
( store, readConfig )
where

import           Types

import           Control.Applicative
import           Control.Distributed.Process
import           Control.Monad.State
import           Data.Map                     (elems, keys, lookup)
import           Prelude                      hiding (lookup)
import           Process.Configurator.Message
import           Process.Configurator.Yaml

readConfig :: IO (Either String (), Monitoring, Log)
readConfig = runGns (StartOptions "gnc.yaml") emptyMonitoring $ do
    parseConfig

type StoreT a = StateT Monitoring Process a

store :: Process ()
store = do
    register "store" =<< getSelfPid
    (_, m, _) <- liftIO $ readConfig
    nsend "cron" (_periodMap m)
    evalStateT storeT m

storeT :: StoreT ()
storeT = forever $ do
    m <- get
    lift $ receiveTimeout 100000 $! map (\f -> f m) [tr, ch, pm, cch]

tr :: Monitoring -> Match ()
tr st = match fun
     where
     fun (Request (pid, TriggerId x)) = do
         send pid $ Response (lookup (TriggerId x) (_triggers st))

ch :: Monitoring -> Match ()
ch st = match fun
     where
     fun (Request (pid, CheckId x)) = do
         send pid $ Response (lookup (CheckId x) (_checks st))

pm :: Monitoring -> Match ()
pm st = match fun
     where
     fun (Request (pid, Cron x)) = do
         send pid $ Response (lookup (Cron x) (_periodMap st))

cch :: Monitoring -> Match ()
cch st = match fun
     where
     fun (Request (pid, CheckHost x)) = do
         send pid $ Response (lookup (CheckHost x) (_checkHost st))

{--
         TriggerId _ -> do
             st <- _triggers <$> get
             lift . send pid $ Response (lookup id st)
         CheckId _ -> do
             st <- (elems . _checks) <$> get
             lift . send pid $ Response (lookup id st)
         Cron _ -> do
             st <- (elems . _periodMap) <$> get
             lift . send pid $ Response (lookup id st)
         CheckHost _ -> do
             st <- (elems . _checkHost) <$> get
             lift . send pid $ Response (lookup id st)

         GetHosts pid -> do
             st <-  (elems . _hosts) <$> get
             lift . send pid $ SMes self st
         GetHostsId pid -> do
             st <- (keys . _hosts) <$> get
             lift . send pid $ SMes self st
         GetHost pid hid -> do
             st <- _hosts <$> get
             lift . send pid $ SMes self $ lookup hid st
         GetCheck pid hid -> do
             st <- _checks <$> get
             lift . send pid $ SMes self $ lookup hid st
         GetTrigger pid hid -> do
             st <- _triggers <$> get
             lift . send pid $ SMes self $ lookup hid st
         GetCronMap pid -> do
             st <-  _periodMap <$> get
             lift . send pid $ SMes self st
         _ -> (lift . say $ "bad message to store") >> return ()

--}


