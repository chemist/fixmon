{-# LANGUAGE ScopedTypeVariables #-}
module Process.Tasker
( tasker
, doTasks
) where


import           Process.Configurator                                (Update (..), getCheckMap, getHostMap)
import           Types

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Data.Set                                            (Set,
                                                                      toList)
import           Data.Vector                                         (Vector,
                                                                      (!))
import           Prelude                                             hiding
                                                                      (lookup)

tasker :: Process ()
tasker = serve () initServer server

taskerName :: Recipient
taskerName = Registered "tasker"

doTasks :: Set CheckHost -> Process ()
doTasks = cast taskerName

data Tasker = Tasker
  { hosts  :: Vector Hostname
  , checks :: Vector Check
  }

initServer :: InitHandler () Tasker
initServer _ = do
    say "start tasker"
    -- register "tasker" =<< getSelfPid
    hm <- getHostMap
    cm <- getCheckMap
    return $ InitOk (Tasker hm cm) Infinity

server :: ProcessDefinition Tasker
server = defaultProcess
    { apiHandlers = [ taskSet
                    ]
    , infoHandlers = [updateConfig]
    }

taskSet :: Dispatcher Tasker
taskSet = handleCast fun
    where
    fun :: Tasker -> Set CheckHost -> Process (ProcessAction Tasker)
    fun st x = do
        mapM_ (startCheck st) $ toList x
        say . show . toList $ x
        continue st

startCheck :: Tasker -> CheckHost -> Process ()
startCheck st ch = do
    let host'' = hosts st ! h ch
        check'' = checks st ! c ch
    say $ "do check: " ++ show check''
    say $ "in host: " ++ show host''
    where
        h :: CheckHost -> Int
        h (CheckHost (i, _)) = unId i

        c :: CheckHost -> Int
        c (CheckHost (_, i)) = unId i

updateConfig :: DeferredDispatcher Tasker
updateConfig = handleInfo $ \_ Update  -> do
    hm <- getHostMap
    cm <- getCheckMap
    continue (Tasker hm cm)
