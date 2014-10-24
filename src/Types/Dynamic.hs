{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Dynamic
( Dyn(..)
, Dynamic(..)
, Env(..)
, runTrigger
, Counter(..)
, ETrigger
, Complex(..)
, Table(..)
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
import           Data.Aeson hiding (Bool)
import           Data.Binary
import           Data.Monoid          (Monoid)
import           Data.String
import           Data.Text            (Text)
import           Data.Text.Binary     ()
import           Data.Time
import           Data.Typeable
import           GHC.Generics


data Dyn = Int Int
         | Double Double
         | Bool Bool
         | Text Text
         | Time UTCTime
         | DynList [Dyn]
         deriving (Show, Eq, Typeable, Ord)

instance ToJSON Dyn where
    toJSON (Int x) = toJSON x
    toJSON (Double x) = toJSON x
    toJSON (Text x) = toJSON x
    toJSON (Bool x) = toJSON x
    toJSON (Time x) = toJSON x
    toJSON (DynList x) = toJSON x

class (Typeable a, Show a, Eq a, Ord a) => Dynamic a where
    toDyn :: a -> Dyn
    fromDyn :: Dyn -> a

instance Dynamic Int where
    toDyn x = Int x
    fromDyn (Int x) = x
    fromDyn _ = undefined

instance Dynamic Double  where
    toDyn x = Double  x
    fromDyn (Double  x) = x
    fromDyn _ = undefined

instance Dynamic Bool  where
    toDyn x = Bool  x
    fromDyn (Bool  x) = x
    fromDyn _ = undefined

instance Dynamic Text  where
    toDyn x = Text  x
    fromDyn (Text  x) = x
    fromDyn _ = undefined

instance Dynamic UTCTime  where
    toDyn x = Time  x
    fromDyn (Time  x) = x
    fromDyn _ = undefined

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


newtype Counter = Counter Text deriving (Eq, Show, Ord, Typeable, Generic, Monoid, IsString)


instance ToJSON Counter where
   toJSON (Counter x) = toJSON x

newtype Table = Table Text deriving (Eq, Show, Ord, Typeable, Generic)
newtype Complex = Complex [(Counter, Dyn)] deriving (Eq, Show, Ord, Typeable, Generic, Monoid)

instance Binary Counter
instance Binary Table

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
    last' <- evalVal (Last c (Count 1))
    prev <- evalVal (Prev c)
    return $ last' /= prev

evalVal :: DynExp -> Eval Dyn
evalVal (Val c) = return c
evalVal (EnvVal c) = evalVal (Last c (Count 1))
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



