{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Storage
( storage
, saveResult
)
where


import           Control.Distributed.Process                         (Process,
                                                                      liftIO,
                                                                      say, spawnLocal)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time

import           Types                                 
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

initServer :: InitHandler db (ST db)
initServer conf = do
    say "start storage"
    return $! InitOk (ST conf []) defDelay

server :: Database db => ProcessDefinition (ST db)
server = defaultProcess
    { apiHandlers = [ saveToQueue ]
    , timeoutHandler = \st _ -> do
        !_ <- spawnLocal $! liftIO $ saveData (database st) (queue st)
        timeoutAfter_ defDelay (st { queue = [] })
    , infoHandlers = []
    , unhandledMessagePolicy = Log
    }


saveToQueue :: Dispatcher (ST db)
saveToQueue = handleCast $ \st (message :: (Hostname, Complex)) -> continue $! st { queue = message : queue st }

{--
c = Complex (fromList [("system.hostname",Any $ Text "limbo-air"), ("system.loadavg", Any $ Int 10)])

cc :: [Series]
cc = map (\x -> Series "newlimbo" (complexToSeriesData $ Complex (fromList [("system.hostname", Any $ Text "limbo-air"), ("system.loadavg", Any $ Int x)]))) $ [1 .. 5000]

s = Series "limbo" (complexToSeriesData c)

-- series :: Value
-- series = toJSON [ 1::Int, 2,3 ]
--}
