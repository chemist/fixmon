{-# LANGUAGE OverloadedStrings #-}
module Checks where

import           Check.Http
import           Check.System
import           Types

import           Data.Dynamic
import           Data.Map.Strict     (unions, fromList)
import           Data.Text    (Text)
import           System.Cron


checkRoutes :: RouteCheck
checkRoutes =
    let http   = map routeCheck [ HttpSimple ]
--        shell  = map routeCheck [ Shell ]
        system' = map routeCheck [ HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
        all' = system' ++ http -- ++ shell
    in all' `seq` unions all'

routes :: Route
routes =
    let system' = map route [HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
        http = map route  [HttpSimple]
 --       shell = map route [Shell]
        all' = system' ++ http --  ++ shell
    in all' `seq` unions all'



testHttp, testHttp1, testShell :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" (fromList [ ("url", toDyn ("http://ya.ru" :: Text)) ])
testHttp1 = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", toDyn ("http://ubank.ru":: Text)), ("redirects", toDyn (2 :: Int))])
testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", toDyn ("" :: Text)), ("command", toDyn ("uptime" :: Text))])
