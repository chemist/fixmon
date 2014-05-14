{-# LANGUAGE OverloadedStrings #-}
module Check.Http where

-- import Network.Wreq
import Data.Text (Text)

import Check
import Types

data Http = HttpSimple deriving Show

instance Checkable Http where
    describe (HttpSimple) = [("url", True, checkUrl), ("agent", False, checkAgent)]
    route (HttpSimple) = ("http.simple", doHttp)

checkUrl :: Text -> Maybe Text
checkUrl = undefined

checkAgent :: Text  -> Maybe Text
checkAgent = undefined

checkAuth :: Text -> Maybe Text
checkAuth = undefined

doHttp :: Check -> IO Complex
doHttp = undefined
