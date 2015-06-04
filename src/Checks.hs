{-# LANGUAGE OverloadedStrings #-}
module Checks where

import           Check.Http
import           Check.Snmp
import           Data.Yaml
import           System.Cron
import           Types

import           Data.Map.Strict (unions)


checkRoutes :: RouteCheck
checkRoutes =
    let http   = map routeCheck [ HttpSimple ]
--        shell  = map routeCheck [ Shell ]
        system' = map routeCheck [ Snmp "system.disk" diskOid Nothing, Snmp "network.interface" interfacesOid Nothing ]
        all' = system' ++ http -- ++ snmp -- ++ shell
    in unions all'

routes :: Route
routes =
    let http = map route  [HttpSimple]
        snmp' = map route [ Snmp "system.disk" diskOid Nothing, Snmp "network.interface" interfacesOid Nothing ]
 --       shell = map route [Shell]
        all' = snmp' ++ http --  ++ snmp --  ++ shell
    in unions all'

testHttp, testHttp1, testShell:: Check
testHttp = Check (CheckName "web") (Hostname "ya.ru") (Cron daily) "http.simple" Nothing $ object [ ("url", (String "http://ya.ru")) ]
testHttp1 = Check (CheckName "web") (Hostname "ubank.ru") (Cron daily) "http.status" Nothing $ object [("url", String "http://ubank.ru"), ("redirects", Number 2 )]
testShell = Check (CheckName "shell") (Hostname "localhost") (Cron daily) "cmd.run" Nothing $ object [("abc", String "" ), ("command", String "uptime")]

-- testSnmp = Check (CheckName "net") (Hostname "salt") (Cron daily) "snmp.network.interface" $ object [ ("community", String "helloall" ), ("host", String "salt" ) ]
