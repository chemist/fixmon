{-# LANGUAGE OverloadedStrings #-}
module Process.Web where

import           Control.Monad.Reader
import           Data.Monoid                                ((<>))
import           Data.Text                                  (Text)
import           Data.Text.Lazy                             (pack)
import           Data.Vector                                (Vector)
import           Data.Vector.Binary                         ()
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp                   (run)
import           Network.Wai.Middleware.RequestLogger       (logStdoutDev)
import           Network.Wai.Middleware.Static
import           Network.Wai
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types
import           Control.Distributed.Process.Node

import qualified Process.Utils                              as PU
import           Types ()


web :: LocalProcess -> IO ()
web lp = run 3000 $ logStdoutDev $ staticHandler $ frontend lp


staticHandler :: Middleware
staticHandler = staticPolicy (noDots >-> addBase "static")

frontend :: LocalProcess -> Application
frontend lp = const . return $ responseFile status200 [] "static/index.html" Nothing

