{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Check.Snmp where

import           Control.Exception
import           Data.ByteString       (ByteString)
import           Data.List             hiding (stripPrefix, lookup)
import           Data.Map.Strict       (singleton, fromList, Map, lookup, empty)
import           Data.Text             (unpack, pack)
import qualified Data.Yaml             as A
import           Network.Protocol.Snmp hiding (Value)
import           Network.Snmp.Client
import           System.Cron
import           Types
import Prelude hiding (lookup)
import Data.Monoid ((<>))

-- import Debug.Trace

interfacesOid :: ByteString
interfacesOid = "1.3.6.1.2.1.2.2.1"

diskOid :: ByteString
diskOid = "1.3.6.1.2.1.25.2.3.1"

data Snmp = Snmp Counter ByteString (Maybe Suite)
            deriving Show

instance Checkable Snmp where
    describe (Snmp _ _ _) = []
    route (Snmp name oi _) = singleton name $ doSnmp (Snmp name oi Nothing)
    routeCheck a@(Snmp x _ _) = routeCheck' a x

doSnmp :: Snmp -> Check -> IO Complex
doSnmp (Snmp c oi _) (Check _ (Hostname host) _ _ (Just conf) _p) = do
    r <- bracket (client (conf { hostname = unpack host }))
                close
                (flip bulkwalk [oidFromBS oi])
    return $ complex (Snmp c oi (Just r))
doSnmp _ (Check _ _ _ _ _ _) = error "oops"

instance ToComplex Snmp where
    complex (Snmp name oi (Just (Suite suite))) =
        let size = length (oidFromBS oi)
            shorted = map conv suite
            conv (Coupla o v) = 
              let [t, i] = drop size o
                  Just (snmpName, convFun) = lookup t (aliasDict name)
              in (snmpName <> "." <> pack (show i), convFun v)
        in A.object shorted
    complex _ = error ""

aliasDict :: Counter -> Map Integer (Counter, Value -> A.Value)
aliasDict "system.disk" = fromList
  [ (1, ("index", (`to` AsInt)))
  , (2, ("id"   , (`to` AsText)))
  , (3, ("descr", (`to` AsText)))
  , (4, ("size" , (`to` AsInt)))
  , (5, ("used" , (`to` AsInt)))
  , (6, ("free" , (`to` AsInt)))
  ]
aliasDict "network.interfaces" = fromList
  [ (1,  ("index", flip to AsInt))
  , (2,  ("id", flip to AsLatinText))
  , (3,  ("name", flip to AsLatinText))
  , (4,  ("mtu", flip to AsInt))
  , (5,  ("speed", flip to AsInt))
  , (6,  ("physAddress", flip to AsMac))
  , (7,  ("adminStatus", flip to AsStatus))
  , (8,  ("operStatus", flip to AsStatus))
  , (9,  ("lastChange", flip to AsInt))
  , (10, ("inOctets", flip to AsInt))
  , (11, ("inUcastPkts", flip to AsInt))
  , (12, ("inNUcastPkts", flip to AsInt))
  , (13, ("inDiscards", flip to AsInt))
  , (14, ("inErrors", flip to AsInt))
  , (15, ("inUnknownProtos", flip to AsInt))
  , (16, ("inOutOctets", flip to AsInt))
  , (17, ("inOutUcastPkts", flip to AsInt))
  , (18, ("inOutNUcastPkts", flip to AsInt))
  , (19, ("inOutDiscards", flip to AsInt))
  , (20, ("inOutErrors", flip to AsInt))
  , (21, ("inOutQlen", flip to AsInt))
  ]
aliasDict _ = empty
 
{--
addAliases :: A.Value -> [Disk] -> [Disk]
addAliases = error "addAliases, not implemented"
addAliases :: A.Value -> [Disk] -> [Disk]
addAliases (A.Object p) d =
    let aliasesOnly = mapMaybe getAlias p
        getAlias (a, String b) = case stripPrefix "alias." a of
                                       Nothing -> Nothing
                                       Just x -> Just (String $ encodeUtf8 b,String $ encodeUtf8 x)
        getAlias _ = error "bad type in addAliases"
    in case lookup (storageId d) aliasesOnly of
            Nothing -> d
            Just x -> d { storageId = x }

--}
testSnmp :: Check
testSnmp = Check (CheckName "test") (Hostname "salt") (Cron daily) "snmp" (Just testConf)$ A.object []

testConf :: Config
testConf = ConfigV3 {hostname = "salt", port = "161", timeout = 5000000, sequrityName = "aes", authPass = "helloallhello", privPass = "helloallhello", sequrityLevel = AuthPriv, context = "", authType = SHA, privType = AES}

{--
data Interface = Interface
  { ifIndex           :: Value
  , ifDescr           :: Value
  , ifType            :: Value
  , ifMtu             :: Value
  , ifSpeed           :: Value
  , ifPhysAddress     :: Value
  , ifAdminStatus     :: Value
  , ifOperStatus      :: Value
  , ifLastChange      :: Value
  , ifInOctets        :: Value
  , ifInUcastPkts     :: Value
  , ifInNUcastPkts    :: Value
  , ifInDiscards      :: Value
  , ifInErrors        :: Value
  , ifInUnknownProtos :: Value
  , ifOutOctets       :: Value
  , ifOutUcastPkts    :: Value
  , ifOutNUcastPkts   :: Value
  , ifOutDiscards     :: Value
  , ifOutErrors       :: Value
  , ifOutQLen         :: Value
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
    complex i = A.object
      [ ("index", to (storageIndex i) AsInt)
      , ("id", to (storageId i) AsText)
      , ("descr", to (storageDescr i) AsText)
      , ("size", to (storageSize i) AsInt)
      , ("used", to (storageUsed i) AsInt)
      , ("free", to (storageFree i) AsInt)
      ]
    convert xs = map convD (groupCoupla xs)

instance ToComplex Interface where
    complex i = A.object
      [ ("index", to (ifIndex i) AsInt)
      , ("id", to (ifDescr i) AsLatinText)
      , ("name", to (ifDescr i) AsLatinText)
      , ("mtu", to (ifMtu i) AsInt)
      , ("speed", to (ifSpeed i) AsInt)
      , ("physAddress", to (ifPhysAddress i) AsMac)
      , ("adminStatus", to (ifAdminStatus i) AsStatus)
      , ("operStatus", to (ifOperStatus i) AsStatus)
      , ("lastChange", to (ifLastChange i) AsInt)
      , ("inOctets", to (ifInOctets i) AsInt)
      , ("inUcastPkts", to (ifInUcastPkts i) AsInt)
      , ("inNUcastPkts", to (ifInNUcastPkts i) AsInt)
      , ("inDiscards", to (ifInDiscards i) AsInt)
      , ("inErrors", to (ifInErrors i) AsInt)
      , ("inUnknownProtos", to (ifInUnknownProtos i) AsInt)
      , ("inOutOctets", to (ifOutOctets i) AsInt)
      , ("inOutUcastPkts", to (ifOutUcastPkts i) AsInt)
      , ("inOutNUcastPkts", to (ifOutNUcastPkts i) AsInt)
      , ("inOutDiscards", to (ifOutDiscards i) AsInt)
      , ("inOutErrors", to (ifOutErrors i) AsInt)
      , ("inOutQlen", to (ifOutQLen i) AsInt)
      ]
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


--}

