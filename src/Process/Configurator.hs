{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveDataTypeable          #-}
module Process.Configurator
( store
, getCronMap
, getCheckMap
, getHostMap
, triggerById
, checkById
, cronSetByCron
, checkHostById
, Update(..)
)
where

import           Process.Configurator.Yaml
import           Types

import           Control.Distributed.Process (Process, say, register, getSelfPid, nsend)
import           Control.Distributed.Process.Platform (Recipient(..))
import           Control.Distributed.Process.Platform.Time
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Monad.State
import           Data.Map                    (lookup, Map)
import           Data.Set                    (Set)
import           Data.Time                   (UTCTime)
import           Data.Vector                 ((!?), Vector)
import           Prelude                     hiding (lookup)
import           System.Directory
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary


---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

store :: Process ()
store = serve "fixmon.yaml" initStore server

storeName :: Recipient
storeName = Registered "configurator"

getCronMap :: Process (Map Cron (Set CheckHost))
getCronMap = call storeName CronMap

getCheckMap :: Process (Vector Check)
getCheckMap = call storeName CheckMap

getHostMap :: Process (Vector Hostname)
getHostMap = call storeName HostMap

triggerById :: TriggerId -> Process (Maybe Trigger)
triggerById = call storeName 

checkById :: CheckId -> Process (Maybe Check)
checkById = call storeName

cronSetByCron :: Cron -> Process (Maybe (Set CheckHost))
cronSetByCron = call storeName

checkHostById :: CheckHost -> Process (Maybe (Set TriggerId))
checkHostById = call storeName

---------------------------------------------------------------------------------------------------
-- internal
---------------------------------------------------------------------------------------------------


defDelay :: Delay
defDelay = Delay $ seconds 20

type ST = (Monitoring, UTCTime, FilePath)

data HostMap = HostMap deriving (Generic, Typeable)
data CheckMap = CheckMap deriving (Generic, Typeable)
data CronMap = CronMap deriving (Typeable, Generic)
data Update = Update deriving (Typeable, Generic)

instance Binary HostMap
instance Binary CheckMap
instance Binary CronMap
instance Binary Update

initStore :: InitHandler FilePath ST
initStore f = do
    say "start configurator"
    register "configurator" =<< getSelfPid
    m <- liftIO $ parseConfig f
    time <- liftIO $ getModificationTime f
    return $ either InitStop (\x -> InitOk (x,time, f) defDelay) m



server :: ProcessDefinition ST
server = defaultProcess {
    apiHandlers = [ cronMap
                  , checkMap
                  , hostMap
                  , lookupTrigger
                  , lookupCheck
                  , lookupCronSet
                  , lookupCheckHost
                  ]
    , timeoutHandler = configuratorTimeoutHandler

    }

cronMap :: Dispatcher ST
cronMap = handleCall $ \st@(m,_,_) CronMap  -> say "call cronMap" >> reply (_periodMap m) st

checkMap :: Dispatcher ST
checkMap = handleCall $ \st@(m,_,_) CheckMap  -> say "call checkMap" >> reply (_checks m) st

hostMap :: Dispatcher ST
hostMap = handleCall $ \st@(m,_,_) HostMap  -> say "call hostMap" >> reply (_hosts m) st

lookupTrigger :: Dispatcher ST
lookupTrigger = handleCall fun
  where
  fun :: ST -> TriggerId -> Process (ProcessReply (Maybe Trigger) ST)
  fun st@(m,_,_) i = say "call lookupTrigger" >> reply (_triggers m !? unId i) st

lookupCheck :: Dispatcher ST
lookupCheck = handleCall fun
  where
  fun :: ST -> CheckId -> Process (ProcessReply (Maybe Check) ST)
  fun st@(m,_,_) i = say "call lookupTrigger" >> reply (_checks m !? unId i) st

lookupCronSet :: Dispatcher ST
lookupCronSet = handleCall fun
  where
  fun :: ST -> Cron -> Process (ProcessReply (Maybe (Set CheckHost)) ST)
  fun st@(m,_,_) c = say "call lookupCronSet" >> reply (lookup c (_periodMap m)) st

lookupCheckHost :: Dispatcher ST
lookupCheckHost = handleCall fun
  where
  fun :: ST -> CheckHost -> Process (ProcessReply (Maybe (Set TriggerId)) ST)
  fun st@(m,_,_) ch = say "call lookupCheckHost" >> reply (lookup ch (_checkHost m)) st

configuratorTimeoutHandler :: TimeoutHandler ST
configuratorTimeoutHandler (m,t,f) _ = do
        time <- liftIO $ getModificationTime f
        if t == time 
           then timeoutAfter defDelay (m,t,f)
           else do
               say "file was changed, reload"
               nm <- liftIO $ parseConfig "fixmon.yaml"
               either (bad time) (good t) nm
        where 
          good time new = do
              nsend "cron" Update
              say "configurator (CronMap)-> cron  "
              nsend "tasker" Update
              say "configurator (CheckMap)-> tasker  "
              timeoutAfter defDelay (new,time,f)
          bad _ e = do
              say $ "configurator bad config" ++ show e
              timeoutAfter defDelay (m,t,f)

