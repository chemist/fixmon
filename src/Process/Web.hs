{-# LANGUAGE OverloadedStrings #-}
module Process.Web where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import           Network.HTTP.Types.Status
import Control.Monad.Reader
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Vector.Binary ()
import Data.Monoid ((<>))
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- import           Network.Wai.Middleware.Static
import Control.Distributed.Process.Node
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types

import qualified Process.Utils as PU
import Types


web :: LocalProcess -> ScottyM ()
web lp = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    b <-  liftIO $ runLocalProcess lp $ PU.request PU.HostMap :: ScottyM (Maybe (Vector Hostname))
    get "/" $ do
        html $ "<p>" <> pack (show b) <> "</p>"

