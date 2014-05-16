{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Check
import           Check.Http
import           System.Cron
import Data.Map (fromList, empty)
import           Types       (Check (..), CheckName (..), Complex (..),
                              Cron (..))

main = print "hello"

testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", "http://www.ubank.neta") ])
                                                                      --   , ("redirects", "3") ])
