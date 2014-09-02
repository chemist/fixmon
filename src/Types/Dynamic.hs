{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Dynamic where

import qualified Data.Dynamic as D
import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text, empty)
import Data.Text.Binary ()
import Data.Time
import Control.Monad.Reader
import Control.Monad.Except


data Dyn = Dyn D.Dynamic
         | DynList [Dyn]

class (Typeable a, Show a, Eq a, Ord a) => Dynamic a where
    toDyn :: a -> Dyn
    toDyn = Dyn . D.toDyn
    fromDyn :: Dyn -> a

instance Dynamic Int where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Int)
    fromDyn _ = undefined

instance Dynamic Text where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Text)
    fromDyn _ = undefined

instance Dynamic Bool where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Bool)
    fromDyn _ = undefined

instance Dynamic Double where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: Double)
    fromDyn _ = undefined

instance Dynamic UTCTime where
    fromDyn (Dyn x) = D.fromDyn x (undefined :: UTCTime)
    fromDyn _ = undefined

instance Show Dyn where
    show (Dyn x)  
        | D.dynTypeRep x == typeInt    = show $ D.fromDyn x (undefined :: Int)
        | D.dynTypeRep x == typeDouble = show $ D.fromDyn x (undefined :: Double)
        | D.dynTypeRep x == typeText   = show $ D.fromDyn x (undefined :: Text)
        | D.dynTypeRep x == typeBool   = show $ D.fromDyn x (undefined :: Bool)
        | D.dynTypeRep x == typeTime   = show $ D.fromDyn x (undefined :: UTCTime)
        | otherwise = error "bad show"
    show (DynList x) = "DynList " ++ show x

instance Eq Dyn where
    xx@(Dyn x) == yy@(Dyn y)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeInt    = (fromDyn xx :: Int) == (fromDyn yy :: Int)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeText   = (fromDyn xx :: Text) == (fromDyn yy :: Text)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeDouble = (fromDyn xx :: Double) == (fromDyn yy :: Double)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeBool   = (fromDyn xx :: Bool) == (fromDyn yy :: Bool)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeTime   = (fromDyn xx :: UTCTime) == (fromDyn yy :: UTCTime)
          | otherwise = error "bad eq"
    DynList x == DynList y = x == y
    _ == _ = error "bad eq"

instance Ord Dyn where
    compare xx@(Dyn x) yy@(Dyn y)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeInt = compare (fromDyn xx :: Int) (fromDyn yy :: Int)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeText = compare (fromDyn xx :: Text) (fromDyn yy :: Text)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeDouble = compare (fromDyn xx :: Double) (fromDyn yy :: Double)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeBool = compare (fromDyn xx :: Bool) (fromDyn yy :: Bool)
          | D.dynTypeRep x == D.dynTypeRep y && D.dynTypeRep x == typeTime = compare (fromDyn xx :: UTCTime) (fromDyn yy :: UTCTime)
          | otherwise = error "bad compare"
    compare (DynList x) (DynList y) = compare x y
    compare _ _ = error "bad compare"

instance Binary Dyn where
    put (Dyn x)
        | D.dynTypeRep x == typeInt    = putWord8 0 >> put (D.fromDyn x (undefined :: Int))
        | D.dynTypeRep x == typeDouble = putWord8 1 >> put (D.fromDyn x (undefined :: Double))
        | D.dynTypeRep x == typeBool   = putWord8 2 >> put (D.fromDyn x (undefined :: Bool))
        | D.dynTypeRep x == typeText   = putWord8 3 >> put (D.fromDyn x (undefined :: Text))
        | D.dynTypeRep x == typeTime   = putWord8 4 >> put (D.fromDyn x (undefined :: UTCTime))
        | otherwise = error "bad binary"
    put (DynList x) = putWord8 5 >> put x
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

typeInt, typeDouble, typeText, typeBool, typeTime :: TypeRep
typeInt = typeOf (1 :: Int)
typeDouble = typeOf (1 :: Double)
typeText = typeOf ( empty :: Text)
typeBool = typeOf ( True :: Bool)
typeTime = typeOf ( undefined :: UTCTime)

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


newtype Counter = Counter Text deriving (Eq, Show, Ord, Typeable, Generic)
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
  , getValue :: Fun -> IO Dyn
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
