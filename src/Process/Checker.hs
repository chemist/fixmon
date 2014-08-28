{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Process.Checker
(checker, doTask)
where


import           Checks
import           Types

import           Control.Distributed.Process                         hiding
                                                                      (call,
                                                                      try)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time

import           Control.Exception
import           Data.Map.Strict                                     (fromList,
                                                                      lookup)
import           Prelude                                             hiding
                                                                      (lookup)

---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

checker :: Process ()
checker = serve () initServer server

doTask :: Recipient -> Check -> Process Complex
doTask = call

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------
initServer :: InitHandler () Route
initServer _ = do
    say "start agent checker"
    return $ InitOk routes Infinity

server :: ProcessDefinition Route
server = defaultProcess
    { apiHandlers = [ taskDispatcher ]
    , infoHandlers = []
    }

taskDispatcher :: Dispatcher Route
taskDispatcher = handleCall $ \st (check :: Check) -> do
    let ch = lookup (ctype check) st
    ch `seq` maybe (notFound st) (doCheck' st check) ch
    where
      notFound = reply (Complex $ fromList [("_status_", Any $ Bool False)])
      doCheck' st check doCheck'' = do
          checkResult <- liftIO $ try $ doCheck'' check
          case checkResult of
               Left (_ :: SomeException) -> reply (Complex $ fromList [("_status_", Any $ Bool False)]) st
               Right r -> reply r st

