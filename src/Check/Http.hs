{-# LANGUAGE OverloadedStrings #-}
module Check.Http where

import           Control.Applicative ((<$>))
-- import           Control.Exception
import           Control.Lens        ((&), (.~), (^.))
import           Data.Dynamic
import           Data.Map.Strict     (fromList, lookup, singleton)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, unpack)
import           Network.URI
import           Network.HTTP.Conduit
import           Prelude             hiding (lookup)
import           Network.HTTP.Types.Status (statusCode)

import           Types


data Http = HttpSimple deriving Show
data Shell = Shell deriving Show

instance Checkable Shell where
    describe Shell = undefined
    route Shell = singleton "cmd.run" undefined
    routeCheck Shell = routeCheck' Shell "cmd.run"

instance Checkable Http where
    describe HttpSimple = [ ("url", True, checkUrl, "Uri - as string, RFC3986")
                          , ("agent", False, checkAgent, "Host - as string, check will be starting from this host, default start from server")
                          , ("redirects", False, checkRedirects, "Count - as integer, default 0")
                          ]
    route HttpSimple = singleton "http.simple" doHttp
    routeCheck HttpSimple = routeCheck' HttpSimple "http.simple"

checkAgent :: Dynamic -> Either String Dynamic
checkAgent x
    | dynTypeRep x == textType = Right x
    | otherwise = Left "bad agent type, must be text"

checkRedirects :: Dynamic -> Either String Dynamic
checkRedirects x
    | dynTypeRep x == intType = Right x
    | otherwise = Left "bad redirects type, must be int"

checkUrl :: Dynamic -> Either String Dynamic
checkUrl x
    | dynTypeRep x == textType = checkUrl' x
    | otherwise = Left "bad url type, must be text"

checkUrl' :: Dynamic -> Either String Dynamic
checkUrl' x = let url = fromDyn x (undefined :: Text)
              in if isAbsoluteURI (unpack url)
                    then Right x
                    else Left "check url, it must be absolute uri, see RFC3986"

doHttp :: Check -> IO Complex
doHttp (Check _ _ _ p) = do
    let Just url = flip fromDyn (undefined :: Text) <$> lookup "url" p
        unpackRedirects :: Dynamic -> Int
        unpackRedirects x = fromDyn x (undefined :: Int)
        redirects' = fromMaybe 0 $ unpackRedirects <$> lookup "redirects" p
    request' <-  parseUrl (unpack url)
    let request = request' 
            { method = "GET"
            , checkStatus = \_ _ _ -> Nothing
            , redirectCount = redirects'
            }
    resp <-  withManager $ \manager -> do
        response <- http request manager
        return $ responseStatus response
    return $ Complex $ fromList [("status", Any $ Int $ statusCode resp)] -- Complex $ fromList [ ("status" , Any $ Int $ resp ^. responseStatus . statusCode) ]

