{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Dynamic where

import qualified Data.Dynamic as D
import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Applicative ((<$>))
import Data.Text (Text, empty)
import Data.Time
import qualified Control.Monad.State as ST
import Data.Map (Map, fromList)


newtype Dyn = Dyn D.Dynamic

class (Typeable a, Show a, Eq a, Ord a) => Dynamic a where
    toDyn :: a -> Dyn
    toDyn = Dyn . D.toDyn
    fromDyn :: Dyn -> a

instance Dynamic Int where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Int)

instance Dynamic Text where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Text)

instance Dynamic Bool where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Bool)

instance Dynamic Double where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Double)

instance Dynamic UTCTime where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: UTCTime)

instance Show Dyn where
    show (Dyn x)  
        | D.dynTypeRep x == typeInt    = show $ D.fromDyn x (undefined :: Int)
        | D.dynTypeRep x == typeDouble = show $ D.fromDyn x (undefined :: Double)
        | D.dynTypeRep x == typeText   = show $ D.fromDyn x (undefined :: Text)
        | D.dynTypeRep x == typeBool   = show $ D.fromDyn x (undefined :: Bool)
        | D.dynTypeRep x == typeTime   = show $ D.fromDyn x (undefined :: UTCTime)
        | otherwise = error "bad type show"

instance Eq Dyn where
    xx@(Dyn x) == yy@(Dyn y)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeInt    = (fromDyn xx :: Int) == (fromDyn yy :: Int)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeText   = (fromDyn xx :: Text) == (fromDyn yy :: Text)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeDouble = (fromDyn xx :: Double) == (fromDyn yy :: Double)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeBool   = (fromDyn xx :: Bool) == (fromDyn yy :: Bool)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeTime   = (fromDyn xx :: UTCTime) == (fromDyn yy :: UTCTime)
          | otherwise = error "bad type"

instance Binary Dyn where
    put (Dyn x)
        | D.dynTypeRep x == typeInt    = putWord8 0 >> put (D.fromDyn x (undefined :: Int))
        | D.dynTypeRep x == typeDouble = putWord8 1 >> put (D.fromDyn x (undefined :: Double))
        | D.dynTypeRep x == typeBool   = putWord8 2 >> put (D.fromDyn x (undefined :: Bool))
        | D.dynTypeRep x == typeText   = putWord8 3 >> put (D.fromDyn x (undefined :: Text))
        | D.dynTypeRep x == typeTime   = putWord8 4 >> put (D.fromDyn x (undefined :: UTCTime))
        | otherwise = error "bad type binary"
    get = fun =<< getWord8 
        where
        fun :: Word8 -> Get Dyn
        fun 0 = toDyn <$> (get :: Get Int)
        fun 1 = toDyn <$> (get :: Get Double)
        fun 2 = toDyn <$> (get :: Get Bool)
        fun 3 = toDyn <$> (get :: Get Text)
        fun 4 = toDyn <$> (get :: Get UTCTime)
        fun _ = error "bad type binary"

typeInt, typeDouble, typeText, typeBool, typeTime :: TypeRep
typeInt = typeOf (1 :: Int)
typeDouble = typeOf (1 :: Double)
typeText = typeOf ( empty :: Text)
typeBool = typeOf ( True :: Bool)
typeTime = typeOf ( undefined :: UTCTime)

class ItInt a where
    yesItInt :: a 
    yesItInt = undefined

class ItBool a where
    yesItBool :: a
    yesItBool = undefined

class ItText a where
    yesItText :: a
    yesItText = undefined

class ItDouble a where
    yesItDouble :: a
    yesItDouble = undefined

class ItTime a where
    yesItTime :: a
    yesItTime = undefined

class ItDyn a where
    yesItDyn :: a
    yesItDyn = undefined

instance ItInt Int 
instance ItBool Bool 
instance ItText Text 
instance ItDouble Double 
instance ItTime Double 

data Simpl = Simpl Dyn deriving (Show, Eq, Generic, Typeable)
 
instance Binary Simpl

data Ev  = S Simpl
         | No Ev Ev 
         | O Ev Ev 
         | An Ev Ev 
         | Les Text Simpl 
         | Mor Text Simpl 
         | Equa Text Simpl 
         deriving (Show, Eq, Typeable, Generic)

instance Binary Ev 

data Evalable a where
  Int :: Int -> Evalable Int
  Bool :: Bool -> Evalable Bool
  Text :: Text -> Evalable Text
  Double :: Double -> Evalable Double
  UTC  :: UTCTime -> Evalable UTCTime

  Not :: Evalable (Bool -> Bool)
  Or ::  Evalable (Bool -> Bool -> Bool)
  And :: Evalable (Bool -> Bool -> Bool)

  Less :: Evalable (Text -> Evalable a -> Bool)
  More :: Evalable (Text -> Evalable a -> Bool)
  Equal :: Evalable (Text -> Evalable a -> Bool)
  Ops :: Evalable (a -> b) -> Evalable a -> Evalable b


instance Binary UTCTime where
    put (UTCTime x y) = put (fromEnum x) >> put (fromEnum y)
    get = do
        x <- get
        y <- get
        return $ UTCTime (toEnum x) (toEnum y)
-- instance Binary a => Binary (Evalable a)
  {--
  Less :: (Evalable Text) -> Dyn -> Evalable Bool
  More :: (Evalable Text) -> Dyn -> Evalable Bool
  Equal :: (Evalable Text) -> Dyn -> Evalable Bool

--}


{--
    -
data Any where
  Any :: (Eq a, Ord a, Show a, Binary a) => !(TriggerRaw a) -> Any
  AnyList :: !([Any]) -> Any

class ToAny a where
    unAny :: Any -> a
    toAny :: a -> Any


--}
