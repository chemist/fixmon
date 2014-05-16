{-# LANGUAGE OverloadedStrings #-}
module Check.Http where

import Network.Wreq
import Data.Text (Text, unpack)
import Data.ByteString.Lazy (ByteString)
import Network.URI
import Data.Map (lookup, fromList)
import Control.Applicative ((<$>))
import Control.Lens ((&), (.~), (^.))
import Control.Exception
import Debug.Trace
import Control.Monad (join)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

import Check
import Types


data Http = HttpSimple deriving Show

instance Checkable Http where
    describe (HttpSimple) = [ ("url", True, "Uri - as string, RFC3986") 
                            , ("agent", False, "Host - as string, check will be starting from this host, default start from server")
                            , ("redirects", False, "Count - as integer, default 0")
                            ]
    route (HttpSimple) = ("http.simple", doHttp)
    isCorrect ch HttpSimple = 
      let url = lookup "url" (cparams ch)
          agent = maybe "" id $ lookup "agent" $ cparams ch
          unpackRedirects :: Text -> Int
          unpackRedirects = fst . head . reads . unpack
          redirects' = maybe 0 id $ unpackRedirects <$> lookup "redirects" (cparams ch)
      in case url of
              Nothing -> Left "url not found"
              Just x -> maybe (Right ch) (Left) $ checkUrl x

checkUrl :: Text -> Maybe Text
checkUrl t = if isAbsoluteURI (unpack t)
                then Nothing
                else Just "bad url, must be absolute url"

doHttp :: Check -> IO Complex
doHttp ch@(Check _ _ _ p) = do
    let Just url = unpack <$> lookup "url" p
        unpackRedirects :: Text -> Int
        unpackRedirects = fst . head . reads . unpack
        Just agent = lookup "agent" p
        redirects' = maybe 0 id $ unpackRedirects <$> lookup "redirects" p
        opts = defaults & redirects .~ redirects'
    r <- try $ getWith opts url 
    either bad good r
    where
      bad :: SomeException -> IO Complex
      bad resp = trace (show resp) $ return $ Complex $ fromList [("success", Any $ Bool False)]
      good :: Response ByteString  -> IO Complex
      good resp = return $ Complex $ fromList [ ("success", Any $ Bool True)
                                              , ("status" , Any $ Int $ resp ^. responseStatus . statusCode)
                                              ]


