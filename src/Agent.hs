{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Check       ()
import           Check.Http  ()
import           Data.Map    (fromList)
import           System.Cron
import           Types       (Check (..), CheckName (..), Cron (..))


main :: IO ()
main = print ("hello" :: String)

testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", "http://www.ubank.neta") ])
                                                                      --   , ("redirects", "3") ])

