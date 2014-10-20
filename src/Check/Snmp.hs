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
    describe SnmpInterfaces = [ ( "community", False, checkString, "community for snmp")
                              , ( "sequrityName", False, checkString, "login for snmp")
                              , ( "version", True, checkVersion, "SNMP version, 2 or 3")
                              , ( "auth", False, checkString, "auth password for SNMPv3")
                              , ( "priv", False, checkString, "priv password for SNMPv3")
                              , ( "authType", False, checkAuthType, "auth type for SNMPv3")
                              , ( "privType", False, checkPrivType, "priv type for SNMPv3")
                              , ( "securityLevel", False, checkSecLevel, "sequrity level for SNMPv3")
                              ]
    route SnmpInterfaces = singleton "snmp.network.interface" doSnmpInterface
    routeCheck SnmpInterfaces = routeCheck' SnmpInterfaces "snmp.network.interface"
    

checkString :: Dyn -> Either String Dyn
checkString = Right

checkVersion :: Dyn -> Either String Dyn
checkVersion x 
  | fromDyn x == (2::Int) = Right x
  | fromDyn x == (3 :: Int) = Right x
  | otherwise = Left "version must be 2 or 3"

checkAuthType :: Dyn -> Either String Dyn
checkAuthType x
  | fromDyn x == ("SHA" :: Text) = Right x
  | fromDyn x == ("MD" :: Text) = Right x
  | otherwise = Left "authType must be SHA or MD"

checkPrivType :: Dyn -> Either String Dyn
checkPrivType x
  | fromDyn x == ("DES" :: Text) = Right x
  | fromDyn x == ("AES" :: Text) = Right x
  | otherwise = Left "privType must be SHA or MD"

checkSecLevel :: Dyn -> Either String Dyn
checkSecLevel x
  | fromDyn x == ("AuthPriv"  :: Text) = Right x
  | fromDyn x == ("AuthNoPriv":: Text) = Right x
  | otherwise = Left "privType must be SHA or MD"

interfacesOid :: ByteString
interfacesOid = "1.3.6.1.2.1.2.2.1"

toV :: Int -> Version
toV 2 = Version2
toV 3 = Version3
toV _ = undefined

toA :: Text -> AuthType
toA "SHA" = SHA
toA "MD" = MD5
toA _ = undefined

toP :: Text -> PrivType
toP "DES" = DES
toP "AES" = AES
toP _ = undefined

toSL :: Text -> PrivAuth
toSL "AuthNoPriv" = AuthNoPriv
toSL "AuthPriv" = AuthPriv
toSL _ = undefined

doSnmpInterface :: Check -> IO Complex
doSnmpInterface (Check _ h _ _ p) = do
    let Just s = encodeUtf8 . fromDyn <$> lookup "sequrityName" p 
        Just a = encodeUtf8 . fromDyn <$> lookup "authPass" p
        Just comm = encodeUtf8 . fromDyn <$> lookup "community" p
        Just pr = encodeUtf8 . fromDyn <$> lookup "privPass" p
        Just at = toA . fromDyn <$> lookup "authType" p
        Just pt = toP . fromDyn <$> lookup "privType" p
        Just sl = toSL . fromDyn <$> lookup "privAuth" p
        Just v = toV . fromDyn <$> lookup "version" p 
        Hostname host = h
        conf = case v of
             Version2 -> (initial Version2) { hostname = unpack host
                                           , community = Community comm
                                           }
             Version3 -> (initial Version3) { hostname = unpack host
                                                  , sequrityName = s
                                                  , authPass = a
                                                  , privPass = pr
                                                  , authType = at
                                                  , privType = pt
                                                  , sequrityLevel = sl
                                                  }
             Version1 -> undefined
    r <- bracket (client conf)
                close
                (\snmp -> bulkwalk snmp [oidFromBS interfacesOid])
    mapM_ (print . complex) $ convert r
    undefined
    
testSnmp :: Check
testSnmp = Check (CheckName "test") (Hostname "salt") (Cron daily) "snmp"
  [ (Counter "sequrityName", toDyn ("aes" :: Text))
  , (Counter "version", toDyn (3 :: Int))
  , (Counter "privType", toDyn ("AES" :: Text))
  , (Counter "privAuth", toDyn ("AuthPriv" :: Text))
  , (Counter "authType", toDyn ("SHA" :: Text))
  , (Counter "authPass", toDyn ("helloallhello" :: Text))
  , (Counter "privPass", toDyn ("helloallhello" :: Text))
  ]

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
      , (iName <> ".name", toDyn $ formatText (ifDescr i))
      , (iName <> ".mtu", toDyn $ formatInt (ifMtu i))
      , (iName <> ".speed", toDyn $ formatInt (ifSpeed i))
      , (iName <> ".physAddress", toDyn $ formatMac (ifPhysAddress i))
      , (iName <> ".adminStatus", toDyn $ formatAdminStatus (ifAdminStatus i))
      , (iName <> ".operStatus", toDyn $ formatOperStatus (ifOperStatus i))
      , (iName <> ".lastChange", toDyn $ formatTimeTicks (ifLastChange i))
      ]
      where iName = "network.interface." <> (Counter $ formatText (ifDescr i)) 
      
phy :: ByteString 
phy = "\224\203NQ\198C"


formatText :: Value -> Text
formatText (String x) = decodeLatin1 x
formatText _ = throw $ BadValue "toText"

formatMac :: Value -> Text
formatMac (String "") = ""
formatMac (String x) = foldl1 (\a b -> a <> ":" <> b) $ T.chunksOf 2 $ pack $ BS.foldl (flip showHex) "" $ BS.reverse x
formatMac _ = throw $ BadValue "toMac"


formatInt :: Value -> Int
formatInt (Integer x) = fromIntegral x
formatInt (Gaude32 x) = fromIntegral x
formatInt _ = throw $ BadValue "toInt"

formatAdminStatus :: Value -> Text
formatAdminStatus (Integer 1) = "up"
formatAdminStatus (Integer 2) = "down"
formatAdminStatus (Integer 3) = "testing"
formatAdminStatus _ = throw $ BadValue "toBool"

formatOperStatus :: Value -> Text
formatOperStatus (Integer 1) = "up"
formatOperStatus (Integer 2) = "down"
formatOperStatus (Integer 3) = "testing"
formatOperStatus (Integer 4) = "unknown"
formatOperStatus (Integer 5) = "dormant"
formatOperStatus (Integer 6) = "notPresent"
formatOperStatus (Integer 7) = "lowerLayerDown"
formatOperStatus _ = throw $ BadValue "formatOperStatus"

formatTimeTicks :: Value -> Int
formatTimeTicks (TimeTicks x) = fromIntegral x
formatTimeTicks _ = throw $ BadValue "formatTimeTicks"


convS :: [Coupla] -> Interface
convS xs =
    let (a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:a16:a17:a18:a19:a20:a21:_) = map value xs
    in Interface a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21



