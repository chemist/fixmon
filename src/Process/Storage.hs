{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Storage
( storage
, saveResult
)
where


import           Control.Distributed.Process                         (Process, catch, 
                                                                      liftIO,
                                                                      say, spawnLocal)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time

import           Types                                 
import           Storage.InfluxDB ()
---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

saveResult :: (Hostname, Complex) -> Process ()
saveResult = cast (Registered "storage")

storage :: Database db => db -> Process ()
storage db = serve db initServer server

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

defDelay :: Delay
defDelay = Delay $ seconds 1

data ST db = ST
  { database ::  db
  , queue    :: ![(Hostname, Complex)]
  }

initServer :: Database db => InitHandler db (ST db)
initServer db = do
    say "start storage"
    return $! InitOk (ST db []) defDelay

server :: Database db => ProcessDefinition (ST db)
server = defaultProcess
    { apiHandlers = [ saveToQueue ]
    , timeoutHandler = \st _ -> do
        _ <- spawnLocal $ (liftIO $ saveData (database st) (queue st)) `catch` (\(e :: DBException) -> say $ show e)
        timeoutAfter_ defDelay (st { queue = [] })
    , infoHandlers = []
    , unhandledMessagePolicy = Log
    }


saveToQueue :: Database db => Dispatcher (ST db)
saveToQueue = handleCast $ \st (message :: (Hostname, Complex)) -> continue $! st { queue = message : queue st }

{--
c = Complex (fromList [("system.hostname",Any $ Text "limbo-air"), ("system.loadavg", Any $ Int 10)])

cc :: [Series]
cc = map (\x -> Series "newlimbo" (complexToSeriesData $ Complex (fromList [("system.hostname", Any $ Text "limbo-air"), ("system.loadavg", Any $ Int x)]))) $ [1 .. 5000]

s = Series "limbo" (complexToSeriesData c)

-- series :: Value
-- series = toJSON [ 1::Int, 2,3 ]
--}
