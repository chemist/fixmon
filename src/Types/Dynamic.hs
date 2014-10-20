{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Dynamic
( dynTypeRep
, Dyn(..)
, Dynamic(..)
, Env(..)
, runTrigger
, Counter(..)
, ETrigger
, Complex(..)
, Table(..)
, iType
, tType
, bType
, dType
, timeType
, Exp(..)
, DynExp(..)
, Fun(..)
, Period(..)
, eval
, evalExp
, DBException(..)
)
where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad.Except
import           Control.Exception
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Binary
import qualified Data.Dynamic         as D
import           Data.Monoid          (Monoid)
import           Data.String
import           Data.Text            (Text)
import           Data.Text.Binary     ()
import           Data.Time
import           Data.Typeable
import           GHC.Generics


data Dyn = Dyn     !D.Dynamic
         | DynList ![Dyn] deriving (Typeable)

dynTypeRep :: Dyn -> TypeRep
dynTypeRep (Dyn x) = D.dynTypeRep x
dynTypeRep (DynList x) = typeOf x

iType, tType, bType, dType, timeType :: TypeRep
iType = typeOf (error "" :: Int)
tType = typeOf (error "":: Text)
bType = typeOf (error "" :: Bool)
dType = typeOf (error "" :: Double)
timeType = typeOf (error "" :: UTCTime)

instance ToJSON Dyn where
    toJSON (Dyn x)
        | D.dynTypeRep x == iType = toJSON $ D.fromDyn x (error "toJson" :: Int)
        | D.dynTypeRep x == dType = toJSON $ D.fromDyn x (error "toJson" :: Double)
        | D.dynTypeRep x == bType = toJSON $ D.fromDyn x (error "toJson" :: Bool)
        | D.dynTypeRep x == tType = toJSON $ D.fromDyn x (error "toJson" :: Text)
        | D.dynTypeRep x == timeType = toJSON $ D.fromDyn x (error "toJson" :: UTCTime)
        | otherwise = error "bad toJson"
    toJSON (DynList x) = toJSON x

class (Typeable a, Show a, Eq a, Ord a) => Dynamic a where
    toDyn :: a -> Dyn
    toDyn = Dyn . D.toDyn
    fromDyn :: Dyn -> a

instance Dynamic Int where
    fromDyn (Dyn x) = D.fromDyn x (error "Dynamic":: Int)
    fromDyn _ = error "fromDyn"

instance Dynamic Text where
    fromDyn (Dyn x) = D.fromDyn x (error "Dynamic":: Text)
    fromDyn _ = error "fromDyn"

instance Dynamic Bool where
    fromDyn (Dyn x) = D.fromDyn x (error "Dynamic":: Bool)
    fromDyn _ = error "fromDyn"

instance Dynamic Double where
    fromDyn (Dyn x) = D.fromDyn x (error "Dynamic":: Double)
    fromDyn _ = error "fromDyn"

instance Dynamic UTCTime where
    fromDyn (Dyn x) = D.fromDyn x (error "Dynamic":: UTCTime)
    fromDyn _ = error "fromDyn"

instance Show Dyn where
    show (Dyn x)
        | D.dynTypeRep x == iType    = show $ D.fromDyn x (error "show dyn" :: Int)
        | D.dynTypeRep x == dType = show $ D.fromDyn x (error "show dyn" :: Double)
        | D.dynTypeRep x == tType   = show $ D.fromDyn x (error "show dyn" :: Text)
        | D.dynTypeRep x == bType   = show $ D.fromDyn x (error "show dyn" :: Bool)
        | D.dynTypeRep x == timeType   = show $ D.fromDyn x (error "show dyn":: UTCTime)
        | otherwise = error "bad show"
    show (DynList x) = "DynList " ++ show x

instance Eq Dyn where
    xx@(Dyn x) == yy@(Dyn y)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == iType      = (fromDyn xx :: Int) == (fromDyn yy :: Int)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == tType      = (fromDyn xx :: Text) == (fromDyn yy :: Text)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == dType      = (fromDyn xx :: Double) == (fromDyn yy :: Double)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == bType      = (fromDyn xx :: Bool) == (fromDyn yy :: Bool)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == timeType   = (fromDyn xx :: UTCTime) == (fromDyn yy :: UTCTime)
          | otherwise = error "bad eq"
    DynList x == DynList y = x == y
    _ == _ = error "bad eq"

instance Ord Dyn where
    compare xx@(Dyn x) yy@(Dyn y)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == iType    = compare (fromDyn xx :: Int) (fromDyn yy :: Int)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == tType    = compare (fromDyn xx :: Text) (fromDyn yy :: Text)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == dType    = compare (fromDyn xx :: Double) (fromDyn yy :: Double)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == bType    = compare (fromDyn xx :: Bool) (fromDyn yy :: Bool)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == timeType = compare (fromDyn xx :: UTCTime) (fromDyn yy :: UTCTime)
          | otherwise = error "bad compare"
    compare (DynList x) (DynList y) = compare x y
    compare _ _ = error "bad compare"

instance Binary Dyn where
    {-# INLINE put #-}
    put (Dyn x)
        | D.dynTypeRep x == iType      = putWord8 0 >> put (D.fromDyn x (error "Binary Dyn" :: Int))
        | D.dynTypeRep x == dType      = putWord8 1 >> put (D.fromDyn x (error "Binary Dyn" :: Double))
        | D.dynTypeRep x == bType      = putWord8 2 >> put (D.fromDyn x (error "Binary Dyn" :: Bool))
        | D.dynTypeRep x == tType      = putWord8 3 >> put (D.fromDyn x (error "Binary Dyn" :: Text))
        | D.dynTypeRep x == timeType   = putWord8 4 >> put (D.fromDyn x (error "Binary Dyn" :: UTCTime))
        | otherwise = error "bad binary"
    put (DynList x) = putWord8 5 >> put x
    {-# INLINE get #-}
    get = fun =<< getWord8
        where
        fun :: Word8 -> Get Dyn
        fun 0 = toDyn <$> (get :: Get Int)
        fun 1 = toDyn <$> (get :: Get Double)
        fun 2 = toDyn <$> (get :: Get Bool)
        fun 3 = toDyn <$> (get :: Get Text)
        fun 4 = toDyn <$> (get :: Get UTCTime)
        fun 5 = DynList <$> (get :: Get [Dyn])
        fun _ = error "bad binary"

data Period a = MicroSec { un :: a }
              | Byte { un :: a }
              | Count { un :: a }
              deriving (Show, Eq, Ord, Typeable, Generic, Functor)



instance Binary (Period Int)

data Exp = Not Exp
         | Or  Exp Exp
         | And Exp Exp
         | Less DynExp DynExp
         | More DynExp DynExp
         | Equal DynExp DynExp
         | Change Counter
         | NoData Counter (Period Int)
         deriving (Show, Eq, Typeable, Generic)

data DynExp = EnvVal Counter
            | Val Dyn
            | Avg Counter (Period Int)
            | Last Counter (Period Int)
            | Prev Counter
            | Min Counter (Period Int)
            | Max Counter (Period Int)
            deriving (Show, Eq, Typeable, Generic)

instance Binary DynExp
instance Binary Exp

newtype Counter = Counter Text deriving (Eq, Show, Ord, Typeable, Generic, Monoid, IsString)


instance ToJSON Counter where
   toJSON (Counter x) = toJSON x

newtype Table = Table Text deriving (Eq, Show, Ord, Typeable, Generic)
newtype Complex = Complex [(Counter, Dyn)] deriving (Eq, Show, Ord, Typeable, Generic)

instance Binary Counter
instance Binary Table
instance Binary Complex

data Fun = ChangeFun Counter
         | LastFun Counter (Period Int)
         | AvgFun Counter (Period Int)
         | PrevFun Counter
         | MinFun Counter (Period Int)
         | MaxFun Counter (Period Int)
         | NoDataFun Counter (Period Int)
         deriving (Show, Eq, Ord, Typeable, Generic)

instance Binary Fun

data Env = Env
  { getValue    :: Fun -> IO Dyn
  }

runTrigger :: Env -> ETrigger -> IO (Either DBException Bool)
runTrigger = eval

type Eval = ReaderT Env IO 

eval :: Env -> Exp -> IO (Either DBException Bool)
eval env e = try (runReaderT (evalExp e) env)

evalExp :: Exp -> Eval Bool
evalExp (Equal x y) = do
    a <- evalVal x
    b <- evalVal y
    return $ a == b
evalExp (More x y) = do
    a <- evalVal x
    b <- evalVal y
    return $ a > b
evalExp (Less x y) = do
    a <- evalVal x
    b <- evalVal y
    return $ a < b
evalExp (Not e) = not <$> evalExp e
evalExp (Or e1 e2) = (||) <$> evalExp e1 <*>  evalExp e2
evalExp (And e1 e2) = (&&) <$> evalExp e1 <*> evalExp e2
evalExp (NoData c i) = do
    getFun <- getValue <$> ask 
    r <- liftIO $ getFun (NoDataFun c i)
    case r of
      DynList [] -> return True
      _ -> return False
evalExp (Change c) = do
    last' <- evalVal (Last c (Count 0))
    prev <- evalVal (Prev c)
    return $ last' /= prev



evalVal :: DynExp -> Eval Dyn
evalVal (Val c) = return c
evalVal (EnvVal c) = evalVal (Last c (Count 0))
evalVal (Last c i) = do
        getFun <- getValue <$> ask
        liftIO $ getFun (LastFun c i)
evalVal (Avg c i) = do
    getFun <- getValue <$> ask
    liftIO $ getFun (AvgFun c i)
evalVal (Prev c) = evalVal (Last c (Count 1))
evalVal (Min c i) = do
    getFun <- getValue <$> ask
    liftIO $ getFun (MinFun c i)
evalVal (Max c i) = do
    getFun <- getValue <$> ask
    liftIO $ getFun (MaxFun c i)

type ETrigger = Exp

instance Binary UTCTime where
    put (UTCTime x y) = put (fromEnum x) >> put (fromEnum y)
    get = do
        x <- get
        y <- get
        return $ UTCTime (toEnum x) (toEnum y)

data DBException = HTTPException String
                 | TypeException String
                 | EmptyException 
                 | DBException String deriving (Show, Typeable)

instance Exception DBException 



