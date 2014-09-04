{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Dynamic
( dynTypeRep
, Dyn(..)
, Dynamic(..)
, Env(..)
, Database(..)
, runTrigger
, Counter(..)
, ETrigger
, Complex(..)
, iType
, tType
, bType
, dType
, timeType
, Exp(..)
, Fun(..)
, Period(..)
)
where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Binary
import qualified Data.Dynamic         as D
import           Data.Monoid          (Monoid)
import           Data.String
import           Data.Text            (Text)
import           Data.Text            (pack)
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

data Period = Sec Int
            | Count Int
            deriving (Show, Eq, Ord, Typeable, Generic)

instance Binary Period

data Exp a where
  EnvVal :: Counter -> Exp Dyn                 -- наименование параметра, значение берется из окружения
  Val :: Dyn -> Exp Dyn
  Last   :: Counter -> Period -> Exp Dyn        -- system.cpu.loadavg.la5->last(3) > system.cpu.loadavg.la5#last(2)
  Avg    :: Counter -> Period -> Exp Dyn        -- system.cpu.loadavg.la5#avg(300)
  Prev   :: Counter -> Exp Dyn              -- предыдущее значение
  Min    :: Counter -> Period -> Exp Dyn        -- system.cpu.loadavg.la1#min(300)
  Max    :: Counter -> Period -> Exp Dyn        -- system.cpu.loadavg.la1#max(300)
  Not :: Exp Bool -> Exp Bool
  Or :: Exp Bool -> Exp Bool -> Exp Bool
  And :: Exp Bool -> Exp Bool -> Exp Bool
  Less :: Exp Dyn -> Exp Dyn -> Exp Bool
  More :: Exp Dyn -> Exp Dyn -> Exp Bool
  Equal :: Exp Dyn -> Exp Dyn -> Exp Bool
  Change :: Counter -> Exp Bool                -- system.hostname#change
  NoData :: Counter -> Period -> Exp Bool          -- http.simple.status#nodata(300)

instance Show (Exp a) where
    show (EnvVal x) = "EnvVal " ++ show x
    show (Val x) = "Val " ++ show x
    show (Last x y) = "Last " ++ show x ++ " " ++ show y
    show (Avg x y) = "Avg " ++ show x ++ " " ++ show y
    show (Prev x) = "Prev " ++ show x
    show (Min x y) = "Min " ++ show x ++ " " ++ show y
    show (Max x y) = "Max " ++ show x ++ " " ++ show y
    show (Not x) = "Not " ++ show x
    show (Or x y) = "Or " ++ show x ++ " " ++ show y
    show (And x y) = "And " ++ show x ++ " " ++ show y
    show (Less x y) = "Less " ++ show x ++ " " ++ show y
    show (More x y) = "More " ++ show x ++ " " ++ show y
    show (Equal x y) = "Equal " ++ show x ++ " " ++ show y
    show (Change x) = "Change " ++ show x
    show (NoData x y) = "NoData " ++ show x ++ " " ++ show y

instance Eq (Exp a) where
    x == y = show x == show y


instance Binary (Exp Dyn) where
    {-# INLINE put #-}
    put (EnvVal x)   = putWord8 0  >> put x
    put (Val x)      = putWord8 1  >> put x
    put (Last x y)   = putWord8 9  >> put x >> put y
    put (Avg x y)    = putWord8 10 >> put x >> put y
    put (Prev x)     = putWord8 11 >> put x
    put (Min x y)    = putWord8 12 >> put x >> put y
    put (Max x y)    = putWord8 13 >> put x >> put y
    {-# INLINE get #-}
    get = getWord8 >>= getExp
        where
            getExp 0  = EnvVal <$> get
            getExp 1  = Val    <$> get
            getExp 9  = Last   <$> get <*> get
            getExp 10 = Avg    <$> get <*> get
            getExp 11 = Prev   <$> get
            getExp 12 = Min    <$> get <*> get
            getExp 13 = Max    <$> get <*> get
            getExp _  = error "bad binary"

instance Binary (Exp Bool) where
    {-# INLINE put #-}
    put (Not x)      = putWord8 2  >> put x
    put (Or x y)     = putWord8 3  >> put x >> put y
    put (And x y)    = putWord8 4  >> put x >> put y
    put (Less x y)   = putWord8 5  >> put x >> put y
    put (More x y)   = putWord8 6  >> put x >> put y
    put (Equal x y)  = putWord8 7  >> put x >> put y
    put (Change x)   = putWord8 8  >> put x
    put (NoData x y) = putWord8 14 >> put x >> put y
    {-# INLINE get #-}
    get = getWord8 >>= getExp
        where
            getExp 2  = Not    <$> get
            getExp 3  = Or     <$> get <*> get
            getExp 4  = And    <$> get <*> get
            getExp 5  = Less   <$> get <*> get
            getExp 6  = More   <$> get <*> get
            getExp 7  = Equal  <$> get <*> get
            getExp 8  = Change <$> get
            getExp 14 = NoData <$> get <*> get
            getExp _  = error "bad binary"


newtype Counter = Counter Text deriving (Eq, Show, Ord, Typeable, Generic, Monoid)

instance IsString Counter where
    fromString x = Counter $ pack x

instance ToJSON Counter where
   toJSON (Counter x) = toJSON x

newtype Table = Table Text deriving (Eq, Show, Ord, Typeable, Generic)
newtype Complex = Complex [(Counter, Dyn)] deriving (Eq, Show, Ord, Typeable, Generic)

instance Binary Counter
instance Binary Table
instance Binary Complex

data Fun = ChangeFun Counter
         | LastFun Counter Period
         | AvgFun Counter Period
         | PrevFun Counter
         | MinFun Counter Period
         | MaxFun Counter Period
         | NoDataFun Counter Period
         deriving (Show, Eq, Ord, Typeable, Generic)

instance Binary Fun

data Env = Env
  { lastComplex :: Complex
  , getValue    :: Fun -> IO Dyn
  }

class Database db where
    getData :: db -> Table -> Fun -> IO Dyn

runTrigger :: Env -> ETrigger -> IO (Either String Bool)
runTrigger e rt = eval e rt

type Eval a = ReaderT Env (ExceptT String IO) a

eval :: Env -> Exp Bool -> IO (Either String Bool)
eval env e = runExceptT (runReaderT (evalExp e) env)

evalExp :: Exp Bool -> Eval Bool
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
evalExp (Not e) = not <$> evalExp e
evalExp (Or e1 e2) = (||) <$> evalExp e1 <*>  evalExp e2
evalExp (And e1 e2) = (&&) <$> evalExp e1 <*> evalExp e2


evalVal :: Exp Dyn -> Eval Dyn
evalVal (Val x) = return x
evalVal (EnvVal x) = do
    Complex complex <- lastComplex <$> ask
    case lookup x complex of
         Nothing -> error "bad counter"
         Just r -> return r
evalVal (Last c i)
    | i == (Count 0) = do
        Complex complex <- lastComplex <$> ask
        case lookup c complex of
             Nothing -> error "bad counter"
             Just r -> return r
    | otherwise = do
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

type ETrigger = Exp Bool

instance Binary UTCTime where
    put (UTCTime x y) = put (fromEnum x) >> put (fromEnum y)
    get = do
        x <- get
        y <- get
        return $ UTCTime (toEnum x) (toEnum y)
