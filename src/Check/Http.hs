{-# LANGUAGE OverloadedStrings #-}
module Check.Http where

import           Control.Applicative  ((<$>))
import           Control.Exception
import           Control.Lens         ((&), (.~), (^.))
import           Data.ByteString.Lazy (ByteString)
import           Data.Map             (fromList, lookup, singleton)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text, unpack)
import           Debug.Trace
import           Network.URI
import           Network.Wreq
import           Prelude              hiding (lookup)

import           Types


data Http = HttpSimple deriving Show
data Shell = Shell deriving Show

instance Checkable Shell where
    describe Shell = undefined
    route Shell = singleton "cmd.run" undefined
    isCorrect _ Shell = undefined

instance Checkable Http where
    describe (HttpSimple) = [ ("url", True, "Uri - as string, RFC3986")
                            , ("agent", False, "Host - as string, check will be starting from this host, default start from server")
                            , ("redirects", False, "Count - as integer, default 0")
                            ]
    route (HttpSimple) = singleton "http.simple" doHttp
    isCorrect ch HttpSimple =
      let url = lookup "url" (cparams ch)
      in case url of
              Nothing -> Left "url not found"
              Just x -> maybe (Right ch) Left $ checkUrl x

checkUrl :: Text -> Maybe Text
checkUrl t = if isAbsoluteURI (unpack t)
                then Nothing
                else Just "bad url, must be absolute url"

doHttp :: Check -> IO Complex
doHttp (Check _ _ _ p) = do
    let Just url = unpack <$> lookup "url" p
        unpackRedirects :: Text -> Int
        unpackRedirects = fst . head . reads . unpack
        Just _ = lookup "agent" p
        redirects' = fromMaybe 0 $ unpackRedirects <$> lookup "redirects" p
        opts = defaults & redirects .~ redirects'
    resp <- getWith opts url
    return $ Complex $ fromList [ ("status" , Any $ Int $ resp ^. responseStatus . statusCode) ]


