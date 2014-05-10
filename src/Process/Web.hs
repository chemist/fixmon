{-# LANGUAGE OverloadedStrings #-}
module Process.Web where

import Network.Wai.Handler.Warp (run)
import Network.Wai
import           Network.HTTP.Types.Status
import Control.Monad.Reader
import Data.Vector (Vector)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Vector.Binary ()
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- import           Network.Wai.Middleware.Static
import Control.Distributed.Process.Node
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types

import qualified Process.Utils as PU
import Types


web :: LocalProcess -> Request -> IO Response
web lp x = do
    b <-  runLocalProcess lp $ PU.request PU.HostMap :: IO (Maybe (Vector Hostname))
    return $ responseLBS status200 [] (pack $ show b)

