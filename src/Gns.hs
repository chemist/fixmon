module Gns  (module Gns) where

import           Data.Monoid (mempty)
import           GNS.Data    as Gns
import           GNS.Parser  as Gns
import           GNS.Trigger as Gns
import           Prelude
import Control.Monad.Error

main :: IO ()
main = do
    a <- runGns (StartOptions "gnc.yaml") mempty $ do
        Gns.parseConfig
--         ddd
    print "hello"
    print a

ddd :: Gns ()
ddd = throwError "error"

