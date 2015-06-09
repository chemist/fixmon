{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts      #-}
module Check.Snmp where

import           Control.Exception
import           Data.ByteString       (ByteString)
import           Data.List             hiding (lookup, stripPrefix)
import           Data.Map.Strict       (lookup, singleton, keys, unions)
import qualified Data.Map.Strict as Map
import           Data.Text             (unpack)
import qualified Data.Yaml             as A
import           Network.Protocol.Snmp hiding (Value, oid)
import           Network.Snmp.Client hiding (oid)
import           Prelude               hiding (lookup)
import           System.Cron
import           Types
import Data.Maybe
import Check.Snmp.Snmp

-- import Debug.Trace

interfacesOid :: ByteString
interfacesOid = "1.3.6.1.2.1.2.2.1"

diskOid :: ByteString
diskOid = "1.3.6.1.2.1.25.2.3.1"

newtype Snmp = Snmp Counter deriving Show

instance Checkable Snmp where
    describe _ = []
    route (Rules vRules) (Snmp _) = unions $ map (\x -> singleton x $ doSnmp (fromJust $ lookup x vRules)) $ keys vRules
    routeCheck _ a@(Snmp x) = routeCheck' a x

doSnmp :: SnmpDefinition -> Check -> IO Complex
doSnmp vSnmpDefinition (Check _ (Hostname vHostname) _ _ (Just vConf) _) = do
    r <- bracket (client (vConf { hostname = unpack vHostname }))
                close
                (flip bulkwalk [oid vSnmpDefinition])
    return $ complex vSnmpDefinition r
doSnmp _ (Check _ _ _ _ _ _) = error "oops"

complex :: SnmpDefinition -> Suite -> A.Value
complex vSnmpDefinition (Suite vSuite) =
    let size = length (oid vSnmpDefinition)
        shorted = map rulesToObject $ splitByI $ map conv vSuite
        vId = bId vSnmpDefinition
        splitByI = Map.elems . Map.fromListWith (\[x] y -> x:y) 
        convertAlias x = to . replaceAlias x . convertFun x
        convertTag x = to . replaceTag x
        rulesToObject ((Just vRule, vValue):xs)
          | simple vRule == vId = ("_check_subname_" A..= (convertTag vRule vValue :: A.Value))
                               : (simple vRule A..= (convertAlias vRule  vValue :: A.Value)) 
                               : rulesToObject xs
          | otherwise = (simple vRule A..= (convertAlias  vRule vValue :: A.Value)) 
                      : rulesToObject xs
        rulesToObject ((Nothing, _):xs) = rulesToObject xs
        rulesToObject [] = []
        conv (Coupla o v) =
          let t:i:_ = drop size o
              convertRule = lookup t (names vSnmpDefinition)
          in (i, [(convertRule, v)])
    in A.array $ map A.object shorted 


testSnmp :: Check
testSnmp = Check (CheckName "test") (Hostname "salt") (Cron daily) "snmp" (Just testConf)$ A.object []

testConf :: Config
testConf = ConfigV3 {hostname = "salt", port = "161", timeout = 5000000, sequrityName = "aes", authPass = "helloallhello", privPass = "helloallhello", sequrityLevel = AuthPriv, context = "", authType = SHA, privType = AES}


