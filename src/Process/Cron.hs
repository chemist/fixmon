{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Process.Cron (cron) where

import           Process.Configurator                                (Update (..), getCronMap)
import           Process.Tasker                                      (doTasks)
import           Types

import           Control.Distributed.Process                         (Process,
                                                                      liftIO,
                                                                      say)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.Binary
import           Data.Map                                            (Map,
                                                                      elems, filterWithKey)
import           Data.Set                                            (Set,
                                                                      unions)
import           Data.Time.Clock
import           Data.Typeable                                       (Typeable)
import           GHC.Generics                                        (Generic)
import           System.Cron

---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

cron :: Process ()
cron = serve () initServer server

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

doCron :: Process ()
doCron = cast (Registered "cron") MinuteMessage

defDelay :: Delay
defDelay = Delay $ seconds 20

type ST = Map Cron (Set CheckHost)

data MinuteMessage = MinuteMessage deriving (Typeable, Generic)
instance Binary MinuteMessage

initServer :: InitHandler () ST
initServer _ = do
    say "start cron"
--    register "cron" =<< getSelfPid
    x <-  getCronMap
    return $ InitOk x defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ minuteTask ]
    , timeoutHandler = \s _ -> do
        doCron
        timeoutAfter_ defDelay s
    , infoHandlers = [updateConfig]
    }

minuteTask :: Dispatcher ST
minuteTask = handleCast $ \st MinuteMessage -> do
    now <- liftIO getCurrentTime
    let tasks = filterWithKey (\(Cron x) _ -> scheduleMatches x now) st
    doTasks (unions . elems $ tasks)
    say "cron (Set CheckHost) -> tasker"
    -- say $ "do tasks " ++ (show . unions . elems $ tasks)
    continue st

updateConfig :: DeferredDispatcher ST
updateConfig = handleInfo $ \_ Update  -> getCronMap >>= continue

