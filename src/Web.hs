{-# LANGUAGE OverloadedStrings #-}
module Web where

import           Control.Exception                          (bracket_)
import           Data.Vector.Binary                         ()
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp                   (run)
import           Network.Wai.Middleware.RequestLogger       (logStdoutDev)
import           Network.Wai.Middleware.Static

import           Types                                      ()


web :: IO ()
web = run 3000 $ logStdoutDev $ staticHandler $ frontend 


staticHandler :: Middleware
staticHandler = staticPolicy (noDots >-> addBase "static")

frontend :: Application
frontend  _ respond  = bracket_
  (putStrLn "Accocation")
  (putStrLn "Cleaning")
  $ respond $responseFile status200 [] "static/index.html" Nothing

