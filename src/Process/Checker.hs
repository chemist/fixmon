{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Process.Checker
(checker, doTask)
where


import           Checks
import           Types hiding (Log)

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
checker = serve () (statelessInit Infinity)  server

doTask :: Recipient -> Check -> Process Complex
doTask = call

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------
server :: ProcessDefinition ()
server = statelessProcess
    { apiHandlers = [ taskDispatcher ]
    , infoHandlers = []
    , unhandledMessagePolicy = Log
    }

taskDispatcher :: Dispatcher ()
taskDispatcher = handleCall_ $ \(check :: Check) -> do
    say $ show check
    let ch = lookup (ctype check) routes
    maybe notFound (doCheck' check) ch
    where
      notFound = return (Complex $ fromList [("_status_", Any $ Bool False)])
      doCheck' check doCheck'' = do
          checkResult <- liftIO $ try $ doCheck'' check
          case checkResult of
               Left (_ :: SomeException) -> return (Complex $ fromList [("_status_", Any $ Bool False)]) 
               Right r -> return r 

