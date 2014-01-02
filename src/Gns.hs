module Gns  (module Gns) where

import           Control.Monad.Error
import           GNS.Data              as Gns
import           GNS.Parser            as Gns
import           GNS.Trigger           as Gns
import           Prelude
import Check.Cron

main :: IO ()
main = void . runGns (StartOptions "gnc.yaml") emptyMonitoring $ do
        Gns.parseConfig

main' :: IO (Either String (), Monitoring, Log)
main' = runGns (StartOptions "gnc.yaml") emptyMonitoring $ do
    Gns.parseConfig

ddd :: Gns ()
ddd = throwError "error"

