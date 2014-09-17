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
import           Data.Text (pack)
import           Storage.InfluxDB ()
import           Data.Monoid ((<>))
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
        (liftIO $ saveData (database st) (writeQueue st)) `catch` (\(e :: DBException) -> critical $ show e)
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
runTriggersCheck db queue = saveTriggers db =<< sendAlarm =<< mapM runQ queue
  where
  runQ :: (Hostname, Trigger) -> Process (Hostname, Complex, Either DBException Bool)
  runQ (Hostname h, tr) = (\r -> return (Hostname h, triggerToComplex tr r, r)) =<< (liftIO $ eval (Env (getData db (Table h))) (tresult tr))

sendAlarm :: [(Hostname, Complex, Either DBException Bool)] -> Process [(Hostname, Complex)]
sendAlarm queue = do
    let onlyFalseAndError (_, _, Right True) = False
        onlyFalseAndError _ = True
        problems = cutUnused $ filter onlyFalseAndError queue
    sendAlarmMessagesAbout problems
    return $ cutUnused queue

cutUnused :: [(a,b,c)] -> [(a,b)]
cutUnused = map $ \(x, y, _) -> (x, y)

sendAlarmMessagesAbout :: [(Hostname, Complex)] -> Process ()
sendAlarmMessagesAbout problems = notice $ show problems

saveTriggers :: Database db => db -> [(Hostname, Complex)] -> Process ()
saveTriggers db queue = (liftIO $ saveData db queue) `catch` \(e :: DBException) -> critical $ show e


data CounterType = State
                 | Message

triggerToComplex :: Trigger -> Either DBException Bool -> Complex
triggerToComplex tr (Left e) = Complex [ (toCounter tr State , toDyn False)
                                       , (toCounter tr Message, toDyn $ pack $ show e)
                                       ]
triggerToComplex tr (Right True) = Complex [ (toCounter tr State, toDyn True) ]
triggerToComplex tr (Right False) = Complex [ (toCounter tr State, toDyn False)
                                            , (toCounter tr Message, toDyn $ tdescription tr)
                                            ]

toCounter :: Trigger -> CounterType -> Counter
toCounter tr State = let TriggerName tn = tname tr
                     in Counter $ "trigger." <> tn <> ".status"
toCounter tr Message = let TriggerName tn = tname tr
                       in Counter $ "trigger." <> tn <> ".message"
                    

{--
c = Complex (fromList [("system.hostname",Any $ Text "limbo-air"), ("system.loadavg", Any $ Int 10)])

cc :: [Series]
cc = map (\x -> Series "newlimbo" (complexToSeriesData $ Complex (fromList [("system.hostname", Any $ Text "limbo-air"), ("system.loadavg", Any $ Int x)]))) $ [1 .. 5000]

s = Series "limbo" (complexToSeriesData c)

-- series :: Value
-- series = toJSON [ 1::Int, 2,3 ]
--}
