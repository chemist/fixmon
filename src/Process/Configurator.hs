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
    self <-  lift getSelfPid
    m <- lift $ expect :: StoreT (SMes Int)
    case m of
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
--         GetTrigger pid hid -> do
--             st <- _triggers <$> get
--             lift . send pid $ SMes self $ lookup hid st
         GetCronMap pid -> do
             st <-  _periodMap <$> get
             lift . send pid $ SMes self st
         _ -> (lift . say $ "bad message to store") >> return ()



