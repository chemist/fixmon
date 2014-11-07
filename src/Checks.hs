{-# LANGUAGE OverloadedStrings #-}
module Checks where

import           Check.Snmp
import           Check.Http
import           Types

import           Data.Map.Strict (unions)


checkRoutes :: RouteCheck
checkRoutes =
    let http   = map routeCheck [ HttpSimple ]
--        shell  = map routeCheck [ Shell ]
        system' = map routeCheck [ SnmpInterfaces, SnmpDisk ]
        all' = system' ++ http -- ++ snmp -- ++ shell
    in unions all'

routes :: Route
routes =
    let http = map route  [HttpSimple]
        snmp' = map route [SnmpInterfaces, SnmpDisk ]
 --       shell = map route [Shell]
        all' = snmp' ++ http --  ++ snmp --  ++ shell
    in unions all'


{--
testHttp, testHttp1, testShell, testSnmp  :: Check
testHttp = Check (CheckName "web") (Cron daily) "http.simple" [ ("url", toDyn ("http://ya.ru" :: Text)) ]
testHttp1 = Check (CheckName "web") (Cron daily) "http.status" [("url", toDyn ("http://ubank.ru":: Text)), ("redirects", toDyn (2 :: Int))]
testShell = Check (CheckName "shell") (Cron daily) "cmd.run" [("abc", toDyn ("" :: Text)), ("command", toDyn ("uptime" :: Text))]

testSnmp = Check (CheckName "net") (Cron daily) "snmp.network.interface" [ ("community", toDyn ("helloall" :: Text)), ("host", toDyn ("salt" :: Text)) ]
--}
