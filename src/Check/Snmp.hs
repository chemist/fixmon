{-# LANGUAGE OverloadedStrings #-}
module Check.Snmp where

import Network.Snmp.Client
import Network.Protocol.Snmp
import Types
import Control.Applicative
import Data.Map.Strict (singleton)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Control.Exception
import System.Cron
import Data.List
import Data.Monoid ((<>))
import Numeric

data Snmp = SnmpInterfaces deriving Show

instance Checkable Snmp where
    describe SnmpInterfaces = []
    route SnmpInterfaces = singleton "network.interface" doSnmpInterface
    routeCheck SnmpInterfaces = routeCheck' SnmpInterfaces "network.interface"
    

interfacesOid :: ByteString
interfacesOid = "1.3.6.1.2.1.2.2.1"

doSnmpInterface :: Check -> IO [Complex]
doSnmpInterface (Check _ h _ _ conf _) = do
    let Hostname host = h
        Just c = conf
    r <- bracket (client (c { hostname = unpack host }))
                close
                (flip bulkwalk [oidFromBS interfacesOid])
    return $ map complex $ convert r
    
testSnmp :: Check
testSnmp = Check (CheckName "test") (Hostname "salt") (Cron daily) "snmp" Nothing []

convert :: Suite -> [Interface]
convert (Suite xs) = map convS $ groupBy fun $ sortBy sortFun xs
  where fun :: Coupla -> Coupla -> Bool
        fun x y = fm x == fm y
        sortFun x y = compare (fm x) (fm y)
        fm = last  . oid

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

instance ToComplex Interface where
    complex i = Complex
      [ (iName <> ".index", toDyn $ formatInt (ifIndex i))
      , (iName <> ".id", toDyn $ formatText (ifDescr i))
      , (iName <> ".name", toDyn $ formatText (ifDescr i))
      , (iName <> ".mtu", toDyn $ formatInt (ifMtu i))
      , (iName <> ".speed", toDyn $ formatInt (ifSpeed i))
      , (iName <> ".physAddress", toDyn $ formatMac (ifPhysAddress i))
      , (iName <> ".adminStatus", toDyn $ formatAdminStatus (ifAdminStatus i))
      , (iName <> ".operStatus", toDyn $ formatOperStatus (ifOperStatus i))
      , (iName <> ".lastChange", toDyn $ formatTimeTicks (ifLastChange i))
      , (iName <> ".inOctets", toDyn $ formatInt (ifInOctets i))
      , (iName <> ".inUcastPkts", toDyn $ formatInt (ifInUcastPkts i))
      , (iName <> ".inNUcastPkts", toDyn $ formatInt (ifInNUcastPkts i))
      , (iName <> ".inDiscards", toDyn $ formatInt (ifInDiscards i))
      , (iName <> ".inErrors", toDyn $ formatInt (ifInErrors i))
      , (iName <> ".inUnknownProtos", toDyn $ formatInt (ifInUnknownProtos i))
      , (iName <> ".inOutOctets", toDyn $ formatInt (ifOutOctets i))
      , (iName <> ".inOutUcastPkts", toDyn $ formatInt (ifOutUcastPkts i))
      , (iName <> ".inOutNUcastPkts", toDyn $ formatInt (ifOutNUcastPkts i))
      , (iName <> ".inOutDiscards", toDyn $ formatInt (ifOutDiscards i))
      , (iName <> ".inOutErrors", toDyn $ formatInt (ifOutErrors i))
      , (iName <> ".inOutQlen", toDyn $ formatInt (ifOutQLen i))
      ]
      where 
      iName = "network.interface" 
      
phy :: ByteString 
phy = "\224\203NQ\198C"


formatText :: Value -> Text
formatText (String x) = decodeLatin1 x
formatText _ = throw $ BadValue "toText"
{-# INLINE formatText #-}


formatMac :: Value -> Text
formatMac (String "") = ""
formatMac (String x) = foldl1 (\a b -> a <> ":" <> b) $ T.chunksOf 2 $ pack $ BS.foldl (flip showHex) "" $ BS.reverse x
formatMac _ = throw $ BadValue "toMac"
{-# INLINE formatMac #-}


formatInt :: Value -> Int
formatInt (Integer x) = fromIntegral x
formatInt (Gaude32 x) = fromIntegral x
formatInt (Counter32 x) = fromIntegral x
formatInt x = throw $ BadValue $ "toInt" <> (pack $ show x)
{-# INLINE formatInt #-}

formatAdminStatus :: Value -> Text
formatAdminStatus (Integer 1) = "up"
formatAdminStatus (Integer 2) = "down"
formatAdminStatus (Integer 3) = "testing"
formatAdminStatus _ = throw $ BadValue "toBool"
{-# INLINE formatAdminStatus #-}

formatOperStatus :: Value -> Text
formatOperStatus (Integer 1) = "up"
formatOperStatus (Integer 2) = "down"
formatOperStatus (Integer 3) = "testing"
formatOperStatus (Integer 4) = "unknown"
formatOperStatus (Integer 5) = "dormant"
formatOperStatus (Integer 6) = "notPresent"
formatOperStatus (Integer 7) = "lowerLayerDown"
formatOperStatus _ = throw $ BadValue "formatOperStatus"
{-# INLINE formatOperStatus #-}

formatTimeTicks :: Value -> Int
formatTimeTicks (TimeTicks x) = fromIntegral x
formatTimeTicks _ = throw $ BadValue "formatTimeTicks"
{-# INLINE formatTimeTicks #-}


convS :: [Coupla] -> Interface
convS xs =
    let (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:a16:a17:a18:a19:a20:a21:_) = map value xs
    in Interface a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21
{-# INLINE convS #-}



