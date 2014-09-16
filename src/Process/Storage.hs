{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Process.Storage
( storage
, saveResult
, checkTrigger
)
where


import           Control.Distributed.Process                         (Process, catch, 
                                                                      liftIO,
                                                                      spawnLocal)

import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time

import           Control.Monad (void)
import           Types                                 
import           Data.Text (Text)
import           Storage.InfluxDB ()
---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

saveResult :: (Hostname, Complex) -> Process ()
saveResult = cast (Registered "storage")

checkTrigger :: (Hostname, Trigger) -> Process ()
checkTrigger = cast (Registered "storage")

storage :: Database db => db -> Process ()
storage db = serve db initServer server

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

defDelay :: Delay
defDelay = Delay $ seconds 1

data ST db = ST
  { database ::  db
  , writeQueue    :: ![(Hostname, Complex)]
  , evalQueue :: ! [(Hostname, Trigger)]
  }

initServer :: Database db => InitHandler db (ST db)
initServer db = do
    warning "start storage"
    return $! InitOk (ST db [] []) defDelay

server :: Database db => ProcessDefinition (ST db)
server = defaultProcess
    { apiHandlers = [ saveToQueue, checkTriggerInternal ]
    , timeoutHandler = \st _ -> do
        (liftIO $ saveData (database st) (writeQueue st)) `catch` (\(e :: DBException) -> warning $ show e)
        void $ spawnLocal $ runTriggersCheck (database st) (evalQueue st)
        timeoutAfter_ defDelay (st { writeQueue = [], evalQueue = [] })
    , infoHandlers = []
    , unhandledMessagePolicy = Log
    }


saveToQueue :: Database db => Dispatcher (ST db)
saveToQueue = handleCast $ \st (message :: (Hostname, Complex)) -> continue $! st { writeQueue = message : writeQueue st }

checkTriggerInternal :: Database db => Dispatcher (ST db)
checkTriggerInternal = handleCast $ \st (message :: (Hostname, Trigger)) -> do
    -- warning $ "have message: " ++ show message
    continue $! st { evalQueue = message : evalQueue st }

runTriggersCheck :: Database db => db -> [(Hostname, Trigger)] -> Process ()
runTriggersCheck db queue = mapM_ runQ queue
  where
  runQ (Hostname h, tr) = do
      r <- liftIO $ eval (Env (getData db (Table h))) (tresult tr)
      res r h tr
  res :: (Either DBException Bool) -> Text -> Trigger -> Process ()
  res (Left e) h tr = warning $ "error when eval trigger " ++ show e ++ " hostname " ++ show h ++ " trigger name is " ++ show (tname tr)
  res (Right b) h tr = warning $ "eval trigger for hostname: " ++ show h ++  " " ++ show b ++ " trigger name is " ++ show (tname tr)

{--
c = Complex (fromList [("system.hostname",Any $ Text "limbo-air"), ("system.loadavg", Any $ Int 10)])

cc :: [Series]
cc = map (\x -> Series "newlimbo" (complexToSeriesData $ Complex (fromList [("system.hostname", Any $ Text "limbo-air"), ("system.loadavg", Any $ Int x)]))) $ [1 .. 5000]

s = Series "limbo" (complexToSeriesData c)

-- series :: Value
-- series = toJSON [ 1::Int, 2,3 ]
--}
