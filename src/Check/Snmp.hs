{-# LANGUAGE OverloadedStrings #-}
module Check.Snmp where

import Network.Snmp.Client
import Network.Protocol.Snmp
import Types
import Data.Map.Strict (singleton)
import Data.Text (unpack, stripPrefix)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Control.Exception
import System.Cron
import Data.List hiding (stripPrefix)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
-- import Debug.Trace

data Snmp = SnmpInterfaces 
          | SnmpDisk
          deriving Show

instance Checkable Snmp where
    describe SnmpInterfaces = []
    describe SnmpDisk = []
    route SnmpInterfaces = singleton "network.interface" (doSnmp SnmpInterfaces interfacesOid)
    route SnmpDisk = singleton "system.disk" (doSnmp SnmpDisk diskOid)
    routeCheck SnmpInterfaces = routeCheck' SnmpInterfaces "network.interface"
    routeCheck SnmpDisk = routeCheck' SnmpDisk "system.disk"
    

interfacesOid :: ByteString
interfacesOid = "1.3.6.1.2.1.2.2.1"

diskOid :: ByteString 
diskOid = "1.3.6.1.2.1.25.2.3.1"

doSnmp :: Snmp -> ByteString -> Check -> IO [Complex]
doSnmp s oi (Check _ (Hostname host) _ _ (Just conf) p) = do
    -- print $ "doSnmp start " <> oi
    r <- bracket (client (conf { hostname = unpack host }))
                close
                (flip bulkwalk [oidFromBS oi])
    -- print $ "doSnmp end " <> oi
    return $ case s of
                  SnmpInterfaces -> map complex (convert r :: [Interface])
                  SnmpDisk       -> map (complex . addAliases p) (convert r :: [Disk])
doSnmp _ _ (Check _ _ _ _ _ _) = error "oops"

addAliases :: [(Counter, Dyn)] -> Disk -> Disk
addAliases p d = 
    let aliasesOnly = mapMaybe getAlias p
        getAlias (Counter a, Text b) = case stripPrefix "alias." a of
                                       Nothing -> Nothing
                                       Just x -> Just (String $ encodeUtf8 b,String $ encodeUtf8 x)
        getAlias _ = error "bad type in addAliases"
    in case lookup (storageId d) aliasesOnly of
            Nothing -> d
            Just x -> d { storageId = x }

testSnmp :: Check
testSnmp = Check (CheckName "test") (Hostname "salt") (Cron daily) "snmp" (Just testConf) []

testConf :: Config
testConf = ConfigV3 {hostname = "", port = "161", timeout = 5000000, sequrityName = "aes", authPass = "helloallhello", privPass = "helloallhello", sequrityLevel = AuthPriv, context = "", authType = SHA, privType = AES}

data Interface = Interface
  { ifIndex :: Value
  , ifDescr :: Value
  , ifType  :: Value
  , ifMtu   :: Value
  , ifSpeed :: Value
  , ifPhysAddress :: Value
  , ifAdminStatus :: Value
  , ifOperStatus  :: Value
  , ifLastChange  :: Value
  , ifInOctets :: Value
  , ifInUcastPkts :: Value
  , ifInNUcastPkts :: Value
  , ifInDiscards :: Value
  , ifInErrors :: Value
  , ifInUnknownProtos :: Value
  , ifOutOctets :: Value
  , ifOutUcastPkts :: Value
  , ifOutNUcastPkts :: Value
  , ifOutDiscards :: Value
  , ifOutErrors :: Value
  , ifOutQLen :: Value
  } deriving (Show)

data Disk = Disk
  { storageIndex :: Value
  , storageId    :: Value
  , storageDescr :: Value
  , storageSize  :: Value
  , storageUsed  :: Value
  , storageFree  :: Value
  } deriving (Show)

instance ToComplex Disk where
    complex i = Complex
      [ (dName <> ".index", toDynR (storageIndex i) AsInt)
      , (dName <> ".id", toDynR (storageId i) AsText)
      , (dName <> ".descr", toDynR (storageDescr i) AsText)
      , (dName <> ".size", toDynR (storageSize i) AsInt)
      , (dName <> ".used", toDynR (storageUsed i) AsInt)
      , (dName <> ".free", toDynR (storageFree i) AsInt)
      ]
      where dName = "system.disk"
    convert xs = map convD (groupCoupla xs)

instance ToComplex Interface where
    complex i = Complex
      [ (iName <> ".index", toDynR (ifIndex i) AsInt)
      , (iName <> ".id", toDynR (ifDescr i) AsLatinText)
      , (iName <> ".name", toDynR (ifDescr i) AsLatinText)
      , (iName <> ".mtu", toDynR (ifMtu i) AsInt)
      , (iName <> ".speed", toDynR (ifSpeed i) AsInt)
      , (iName <> ".physAddress", toDynR (ifPhysAddress i) AsMac)
      , (iName <> ".adminStatus", toDynR (ifAdminStatus i) AsStatus)
      , (iName <> ".operStatus", toDynR (ifOperStatus i) AsStatus)
      , (iName <> ".lastChange", toDynR (ifLastChange i) AsInt)
      , (iName <> ".inOctets", toDynR (ifInOctets i) AsInt)
      , (iName <> ".inUcastPkts", toDynR (ifInUcastPkts i) AsInt)
      , (iName <> ".inNUcastPkts", toDynR (ifInNUcastPkts i) AsInt)
      , (iName <> ".inDiscards", toDynR (ifInDiscards i) AsInt)
      , (iName <> ".inErrors", toDynR (ifInErrors i) AsInt)
      , (iName <> ".inUnknownProtos", toDynR (ifInUnknownProtos i) AsInt)
      , (iName <> ".inOutOctets", toDynR (ifOutOctets i) AsInt)
      , (iName <> ".inOutUcastPkts", toDynR (ifOutUcastPkts i) AsInt)
      , (iName <> ".inOutNUcastPkts", toDynR (ifOutNUcastPkts i) AsInt)
      , (iName <> ".inOutDiscards", toDynR (ifOutDiscards i) AsInt)
      , (iName <> ".inOutErrors", toDynR (ifOutErrors i) AsInt)
      , (iName <> ".inOutQlen", toDynR (ifOutQLen i) AsInt)
      ]
      where 
      iName = "network.interface" 
    convert xs = map convS (groupCoupla xs)
    
groupCoupla :: Suite -> [[Coupla]]
groupCoupla (Suite xs) = groupBy fun $ sortBy sortFun xs
      where fun :: Coupla -> Coupla -> Bool
            fun x y = fm x == fm y
            sortFun x y = compare (fm x) (fm y)
            fm = last  . oid

     
phy :: ByteString 
phy = "\224\203NQ\198C"

convD :: [Coupla] -> Disk
convD xs =
    let (a1:_:a3:a4:a5:a6:_) = map value xs
        size = a5 *** a4
        used = a6 *** a4
        free = size `minus` used
    in Disk a1 a3 a3 size used free

(***) :: Value -> Value -> Value
(Integer x) *** (Integer y) = Counter64 $ fromIntegral x * fromIntegral y
(***) _ _ = error "must be Integer here"
minus :: Value -> Value -> Value
(Counter64 x) `minus` (Counter64 y) = Counter64 $ fromIntegral x - fromIntegral y
minus _ _ = error "must be Integer here"

convS :: [Coupla] -> Interface
convS xs =
    let (a1:a2:a3:a4:a5:a6:a7:a8:a9:(Counter32 a10):a11:a12:a13:a14:a15:(Counter32 a16):a17:a18:a19:a20:a21:_) = map value xs
    in Interface a1 a2 a3 a4 a5 a6 a7 a8 a9 (Counter64 $ 8 * fromIntegral a10) a11 a12 a13 a14 a15 (Counter64 $ 8 * fromIntegral a16) a17 a18 a19 a20 a21
{-# INLINE convS #-}



