module Gns  (module Gns) where

import GNS.Data as Gns
import GNS.Trigger as Gns hiding (name)
import GNS.Parser as Gns
import Prelude (print)

main = do
    c <- parseConfig "gnc.yaml"
    print c

