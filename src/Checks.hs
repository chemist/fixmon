{-# LANGUAGE OverloadedStrings #-}
module Checks where

import           Check.Http
import           Check.System
import           Data.Map     (unions)
import           Types

import           Data.Dynamic
import           Data.Map     (fromList)
import           Data.Text    (Text)
import           System.Cron


checkRoutes :: RouteCheck
checkRoutes =
    let http   = map routeCheck [ HttpSimple ]
        shell  = map routeCheck [ Shell ]
        system' = map routeCheck [ HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
    in unions $ http ++ shell ++ system'

routes :: Route
routes =
    let system' = map route [HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
        http = map route  [HttpSimple]
        shell = map route [Shell]
    in unions $ system' ++ http ++ shell



testHttp :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", toDyn ("http://ya.ru" :: Text)) ])

testHttp1 = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", toDyn ("http://ubank.ru":: Text)), ("redirects", toDyn (2 :: Int))])
testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", toDyn ("" :: Text)), ("command", toDyn ("uptime" :: Text))])
