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
, Counter(..)
, Complex(..)
, Table(..)
, Exp(..)
, DynExp(..)
, Fun(..)
, Period(..)
, eval
, evalExp
, DBException(..)
, Rule(..)
, ToDyn(..)
, countersFromExp
)
where

import           Control.Applicative  ((<$>), (<*>))
-- import           Control.Monad.Except
import           Control.Exception
import           Control.Monad.Reader
import           Data.Aeson hiding (Bool)
import           Data.Binary
import           Data.Monoid          (Monoid, (<>))
import           Data.String
import           Data.Text            (Text, chunksOf, pack)
import           Data.Text.Encoding   (decodeUtf8, decodeLatin1)
import qualified Data.ByteString as BS
import           Data.Text.Binary     ()
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import qualified Network.Protocol.Snmp as S
import           Numeric (showHex)


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

data Rule = AsInt
          | AsLatinText
          | AsText
          | AsMac
          | AsStatus
          deriving Show

class (Show a, Eq a) => ToDyn a where
    toDynR :: a -> Rule -> Dyn

instance ToDyn S.Value where
    toDynR (S.Integer x) AsInt = Int . fromIntegral $ x
    toDynR (S.Integer 1) AsStatus = Text "up"
    toDynR (S.Integer 2) AsStatus = Text "down"
    toDynR (S.Integer 3) AsStatus = Text "testing"
    toDynR (S.Integer 4) AsStatus = Text "unknown"
    toDynR (S.Integer 5) AsStatus = Text "dormant"
    toDynR (S.Integer 6) AsStatus = Text "notPresent"
    toDynR (S.Integer 7) AsStatus = Text "lowerLayerDown"
    toDynR (S.Counter32 x) AsInt = Int . fromIntegral $ x
    toDynR (S.Gaude32 x) AsInt = Int . fromIntegral $ x
    toDynR (S.TimeTicks x) AsInt = Int . fromIntegral $ x
    toDynR (S.String x) AsText = Text . decodeUtf8 $ x
    toDynR (S.String x) AsLatinText = Text . decodeLatin1 $ x
    toDynR (S.String "") AsMac = Text ""
    toDynR (S.String x) AsMac = Text . foldr1 (\a b -> a <> ":" <> b) . chunksOf 2 . pack . BS.foldr showHex "" $ x
    toDynR (S.OI x) AsText = Text . pack . foldr (\a b -> show a <> "." <> b) "" $ x
    toDynR (S.Opaque x) AsText = Text . decodeUtf8 $ x
    toDynR (S.Opaque x) AsLatinText = Text . decodeLatin1 $ x
    toDynR (S.Counter64 x) AsInt = Int . fromIntegral $ x
    toDynR x y = error $ show x ++ show y

instance Dynamic Int where
    toDyn x = Int x
    fromDyn (Int x) = x
    fromDyn _ = error "fromDyn int"

instance Dynamic Double  where
    toDyn x = Double  x
    fromDyn (Double  x) = x
    fromDyn _ = error "fromDyn double"

instance Dynamic Bool  where
    toDyn x = Bool  x
    fromDyn (Bool  x) = x
    fromDyn _ = error "fromDyn bool"

instance Dynamic Text  where
    toDyn x = Text  x
    fromDyn (Text  x) = x
    fromDyn _ = error "formDyn text"

instance Dynamic UTCTime  where
    toDyn x = Time  x
    fromDyn (Time  x) = x
    fromDyn _ = error "fromDyn time"

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
evalVal (Prev c) = evalVal (Last c (Count 2))
evalVal (Min c i) = do
    getFun <- getValue <$> ask
    liftIO $ getFun (MinFun c i)
evalVal (Max c i) = do
    getFun <- getValue <$> ask
    liftIO $ getFun (MaxFun c i)

countersFromExp :: Exp -> [Counter]
countersFromExp (Change _) = []
countersFromExp (NoData _ _) = []
countersFromExp (Not x) = countersFromExp x
countersFromExp (Or x y) = countersFromExp x <> countersFromExp y
countersFromExp (And x y) = countersFromExp x <> countersFromExp y
countersFromExp (More x y) = countersFromDyn x <> countersFromDyn y 
countersFromExp (Less x y) = countersFromDyn x <> countersFromDyn y 
countersFromExp (Equal x y) = countersFromDyn x <> countersFromDyn y 

countersFromDyn :: DynExp -> [Counter]
countersFromDyn (EnvVal x) = [x]
countersFromDyn _ = []


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



