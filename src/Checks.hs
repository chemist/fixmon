{-# LANGUAGE OverloadedStrings #-}
module Checks where

import           Check.Http
import           Check.System
import           Check.Snmp
import           Types

import           Data.Map.Strict (unions)
import           Data.Text       (Text)
import           System.Cron


checkRoutes :: RouteCheck
checkRoutes =
    let -- http   = map routeCheck [ HttpSimple ]
--        shell  = map routeCheck [ Shell ]
        system' = map routeCheck [ SnmpInterfaces ]
    --    snmp = map routeCheck [SnmpInterfaces]
        all' = system' -- ++ http -- ++ snmp -- ++ shell
    in all' `seq` unions all'

routes :: Route
routes =
    let -- system' = map route [HostName, Uptime, Boottime, CpuIntr, CpuLoad, CpuInfo, CpuSwitches, CpuUtil, LocalTime]
        -- http = map route  [HttpSimple]
        snmp = map route [SnmpInterfaces ]
 --       shell = map route [Shell]
        all' = snmp -- system' ++ http --  ++ snmp --  ++ shell
    in all' `seq` unions all'


{--
testHttp, testHttp1, testShell, testSnmp  :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" [ ("url", toDyn ("http://ya.ru" :: Text)) ]
testHttp1 = Check (CheckName "web") (Cron daily) "http.status" [("url", toDyn ("http://ubank.ru":: Text)), ("redirects", toDyn (2 :: Int))]
testShell = Check (CheckName "shell") (Cron daily) "cmd.run" [("abc", toDyn ("" :: Text)), ("command", toDyn ("uptime" :: Text))]

testSnmp = Check (CheckName "net") (Cron daily) "snmp.network.interface" [ ("community", toDyn ("helloall" :: Text)), ("host", toDyn ("salt" :: Text)) ]
--}
