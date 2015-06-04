{-# LANGUAGE OverloadedStrings #-}
module Check.Http where

import qualified Data.HashMap.Strict       as HM
import           Data.Map.Strict           (singleton)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (unpack)
import           Data.Yaml
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status (statusCode)
import           Network.URI
import           Prelude

import           Types


data Http = HttpSimple deriving Show
data Shell = Shell deriving Show

instance Checkable Shell where
    describe Shell = error "http check"
    route Shell = singleton "cmd.run" (error "http check")
    routeCheck Shell = routeCheck' Shell "cmd.run"

instance Checkable Http where
    describe HttpSimple = [ ("url", True, checkUrl, "Uri - as string, RFC3986")
                          , ("agent", False, checkAgent, "Host - as string, check will be starting from this host, default start from server")
                          , ("redirects", False, checkRedirects, "Count - as integer, default 0")
                          ]
    route HttpSimple = singleton "http.simple" doHttp
    routeCheck HttpSimple = routeCheck' HttpSimple "http.simple"

checkAgent :: Dyn -> Either String Dyn
checkAgent (String x) = Right (String  x)
checkAgent _ = Left "bad agent type, must be text"

checkRedirects :: Dyn -> Either String Dyn
checkRedirects (Number x) = Right (Number x)
checkRedirects _ = Left "bad redirects type, must be int"

checkUrl :: Dyn -> Either String Dyn
checkUrl (String x) = Right (String x)
checkUrl _ = Left "bad url type, must be text"

checkUrl' :: Dyn -> Either String Dyn
checkUrl' x = let url = from x
              in if isAbsoluteURI (unpack url)
                    then Right x
                    else Left "check url, it must be absolute uri, see RFC3986"

doHttp :: Check -> IO Complex
doHttp (Check (CheckName n) _ _ _ _ (Object p)) = do
    let Just url = from <$> HM.lookup "url" p
        unpackRedirects :: Dyn -> Int
        unpackRedirects x = from x
        redirects' = fromMaybe 0 $ unpackRedirects <$> HM.lookup "redirects" p
    request' <-  parseUrl (unpack url)
    let request = request'
            { method = "GET"
            , checkStatus = \_ _ _ -> Nothing
            , redirectCount = redirects'
            }
    resp <-  withManager $ \manager -> do
        response <- http request manager
        return $ responseStatus response
    return $ object [("id", to n), ("status", to $ statusCode resp)] -- Complex $ fromList [ ("status" , Any $ Int $ resp ^. responseStatus . statusCode) ]
doHttp (Check _ _ _ _ _ _) = error "something was wrong in doHttp"

