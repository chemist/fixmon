{-# LANGUAGE OverloadedStrings #-}
module Process.Web where

import           Control.Distributed.Process.Internal.Types
import           Control.Exception                          (bracket_)
import           Data.Vector.Binary                         ()
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp                   (run)
import           Network.Wai.Middleware.RequestLogger       (logStdoutDev)
import           Network.Wai.Middleware.Static

import           Types                                      ()


web :: LocalProcess -> IO ()
web lp = run 3000 $ logStdoutDev $ staticHandler $ frontend lp


staticHandler :: Middleware
staticHandler = staticPolicy (noDots >-> addBase "static")

frontend :: LocalProcess -> Application
frontend _ _ respond  = bracket_
  (putStrLn "Accocation")
  (putStrLn "Cleaning")
  $ respond $responseFile status200 [] "static/index.html" Nothing

