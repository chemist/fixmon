module Gns  (module Gns) where

import           Control.Monad.Error
import           Data.Monoid           (mempty)
import           GNS.Data              as Gns
import           GNS.Parser            as Gns
import           GNS.Trigger           as Gns
import           Prelude
import Check.Cron

main :: IO ()
main = void . runGns (StartOptions "gnc.yaml") mempty $ do
        Gns.parseConfig
        checker
--         ddd

ddd :: Gns ()
ddd = throwError "error"

