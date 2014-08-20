{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
module Data.Network.Ip where

import           Control.Applicative
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Typeable
import           Data.Word
import           GHC.Generics
import           Numeric
import           Text.ParserCombinators.ReadP hiding (get)
import           Text.Printf                  (printf)

import           Control.Monad                (void)
import           Data.Serialize

showAsBin::(Num a,Integral a, Show a) => a  -> String
showAsBin i = showIntAtBase 2 intToDigit i ""

data Mask = Mask32 Word32
          | Mask128 Word64 Word64 deriving (Eq, Ord)

data IP = IPv4 !Word32
        | IPv6 !Word64 !Word64 deriving (Typeable, Generic, Eq, Ord)

data IPSubnet = IPSubnet IP Mask deriving (Eq, Ord)

class ShowBin a where
    showBin::a -> String

instance ShowBin IP where
    showBin (IPv4 x) = showAsBin x
    showBin _ = undefined

instance ShowBin Mask where
    showBin (Mask32 x) = showAsBin x
    showBin _ = undefined

instance Serialize IP where
    put (IPv4 x) = put (0::Word8) >> put x
    put (IPv6 x y) = put (1::Word8) >> put x >> put y
    get = do m <- get :: Get Word8
             case m of
                  0 -> IPv4 <$> get
                  1 -> IPv6 <$> get <*> get
                  _ -> fail "problem"

instance ShowBin IPSubnet where
    showBin (IPSubnet i _) = showBin i

instance Read IP where
    readsPrec _ = readP_to_S readIP

instance Read Mask where
    readsPrec _ = readP_to_S readMask

instance Read IPSubnet where
    readsPrec _ = readP_to_S readSubnetIPv4

instance Show IP where
    show (IPv4 ip) = printf "%d.%d.%d.%d" a b c d
        where ( _, a) = shift8 r1
              (r1, b) = shift8 r2
              (r2, c) = shift8 r3
              (r3, d) = shift8 ip
              shift8  = (`divMod` 256)
    show _ = undefined

instance Show Mask where
    show (Mask32 m) = show $ popCount m
    show _ = undefined

instance Show IPSubnet where
    show (IPSubnet i m) = show i ++ "/" ++ show m

network::IP -> Mask -> IPSubnet
network (IPv4 i) mm@(Mask32 m) = IPSubnet (IPv4 (i .&. m)) mm
network _ _ = undefined

broadcast::IPSubnet -> IP
broadcast (IPSubnet (IPv4 i) (Mask32 m)) =  IPv4 $ i .|. complement m
broadcast _ = undefined

isHostInNetwork::IP -> IPSubnet -> Bool
isHostInNetwork i s@(IPSubnet _ m) = network i m == s

readSubnetIPv4::ReadP IPSubnet
readSubnetIPv4 = do
    i <- readIP
    void $ char '/'
    m <- maskAsInt
    return $ network i m

readMask::ReadP Mask
readMask = maskAsInt

maskAsInt::ReadP Mask
maskAsInt = do
    a <- fun <$> many1 (satisfy isDigit)
    if a >= 0 && a <= 32
         then return $ Mask32 $ shiftL (shiftR maxMask (32 - a)) (32 - a)
         else pfail
    where fun = fromIntegral . digitsToInt
          maxMask = maxBound::Word32

toMask32::Int -> Maybe Mask
toMask32 i = if i >= 0 && i <= 32
                then Just $ Mask32 $ shiftL (shiftR maxMask (32 - i)) (32 - i)
                else Nothing
           where maxMask = maxBound::Word32


readIP::ReadP IP
readIP = do
    a <- fun <$> many1 (satisfy isDigit)
    void $ char '.'
    b <- fun <$> many1 (satisfy isDigit)
    void $ char '.'
    c <- fun <$> many1 (satisfy isDigit)
    void $ char '.'
    d <- fun <$> many1 (satisfy isDigit)
    if right a && right b && right c && right d
                  then return $ IPv4 (d + a `shift` 24 + b `shift` 16 + c `shift` 8)
                  else pfail
    where fun = fromIntegral . digitsToInt
          right x = x >= 0 && x <= 255

-- |Parse an unsigned integer.
digitsToInt :: String -> Int
digitsToInt = foldl' ((+) . (10 *)) 0 . map digitToInt

