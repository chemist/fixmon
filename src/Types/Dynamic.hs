{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Types.Dynamic
( Dyn
, Env(..)
, Counter
, Complex
, Table(..)
, Exp(..)
, DynExp(..)
, Fun(..)
, Period(..)
, eval
, evalExp
, Convert(..)
, DBException(..)
, Rule(..)
, countersFromExp
)
where

-- import           Control.Monad.Except
import           Control.Exception
import           Control.Monad.Reader
import           Data.Binary
import qualified Data.ByteString       as BS
import           Data.Monoid           ((<>))
import           Data.Scientific
import           Data.Text             (Text, chunksOf, pack)
import           Data.Text.Binary      ()
import           Data.Text.Encoding    (decodeLatin1, decodeUtf8)
import           Data.Time
import           Data.Typeable
import qualified Data.Vector           as V
import           Data.Yaml
import           GHC.Generics          hiding (from)
import qualified Network.Protocol.Snmp as S
import           Numeric               (showHex)


type Dyn = Value

instance Ord Dyn where
    compare Null Null = EQ
    compare (String x) (String y) = compare x y
    compare (Number x) (Number y) = compare x y
    compare (Array x) (Array y) = compare x y
    compare _ _ = LT

class (Typeable a, Show a, Eq a, Ord a) => Convert a b where
    to :: a -> b
    from :: b -> a

data Rule = AsInt
          | AsLatinText
          | AsText
          | AsMac
          | AsStatus
          deriving Show

deriving instance Typeable S.Value
deriving instance Ord S.Value

instance Convert S.Value (Rule -> Dyn) where
    to (S.Integer x) AsInt = Number . fromIntegral $ x
    to (S.Integer 1) AsStatus = String "up"
    to (S.Integer 2) AsStatus = String "down"
    to (S.Integer 3) AsStatus = String "testing"
    to (S.Integer 4) AsStatus = String "unknown"
    to (S.Integer 5) AsStatus = String "dormant"
    to (S.Integer 6) AsStatus = String "notPresent"
    to (S.Integer 7) AsStatus = String "lowerLayerDown"
    to (S.Counter32 x) AsInt = Number . fromIntegral $ x
    to (S.Gaude32 x) AsInt = Number . fromIntegral $ x
    to (S.TimeTicks x) AsInt = Number . fromIntegral $ x
    to (S.String x) AsText = String . decodeUtf8 $ x
    to (S.String x) AsLatinText = String . decodeLatin1 $ x
    to (S.String "") AsMac = String ""
    to (S.String x) AsMac = String . foldr1 (\a b -> a <> ":" <> b) . chunksOf 2 . pack . BS.foldr showHex "" $ x
    to (S.OI x) AsText = String . pack . foldr (\a b -> show a <> "." <> b) "" $ x
    to (S.Opaque x) AsText = String . decodeUtf8 $ x
    to (S.Opaque x) AsLatinText = String . decodeLatin1 $ x
    to (S.Counter64 x) AsInt = Number . fromIntegral $ x
    to x y = error $ show x ++ show y
    from _ = error "cant convert (Rule -> Dyn) to Value"


instance Convert Int Dyn where
    to x = Number . fromIntegral $ x
    from (Number x) = case (floatingOrInteger x :: Either Double Int) of
                           Left _ -> error "from int, float"
                           Right i -> i
    from _ = error "from int"

instance Convert Double Dyn where
    to x = Number  . fromFloatDigits $ x
    from (Number  x) = case (floatingOrInteger x :: Either Double Int) of
                            Left d -> d
                            Right _ -> error "integer when must be float"
    from _ = error "fromDyn double"

instance Convert Bool Dyn where
    to x = Bool  x
    from (Bool  x) = x
    from _ = error "fromDyn bool"

instance Convert Text Dyn where
    to x = String  x
    from (String  x) = x
    from _ = error "formDyn text"

{--
instance Convert UTCTime Dyn  where
    to x = Time  x
    from (Time  x) = x
    from _ = error "fromDyn time"
    --}

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


type Counter = Text -- deriving (Eq, Show, Ord, Typeable, Generic, Monoid, IsString)


newtype Table = Table Text deriving (Eq, Show, Ord, Typeable, Generic)
type Complex = Value -- deriving (Eq, Show, Typeable, Generic)

instance Binary Table

data Fun = ChangeFun Counter
         | LastFun Counter (Period Int)
         | AvgFun Counter (Period Int)
         | PrevFun Counter
         | MinFun Counter (Period Int)
         | MaxFun Counter (Period Int)
         | NoDataFun Counter (Period Int)
         | EnvValFun Counter
         deriving (Show, Eq, Ord, Typeable, Generic)

instance Binary Fun

data Env = Env
  { getValue :: Fun -> IO Dyn
  }

type Eval = ReaderT Env IO

eval :: Env -> Exp -> IO (Either SomeException Bool)
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
      Array x -> if (V.null x) then return True else return False
      _ -> return False
evalExp (Change c) = do
    last' <- evalVal (EnvVal c)
    prev <- evalVal (Prev c)
    return $ last' /= prev

evalVal :: DynExp -> Eval Dyn
evalVal (Val c) = return c
evalVal (EnvVal c) = do
    getFun <- getValue <$> ask
    liftIO $ getFun (EnvValFun c)
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
countersFromDyn (Last x _) = [x]
countersFromDyn (Avg x _) = [x]
countersFromDyn (Prev x) = [x]
countersFromDyn (Min x _) = [x]
countersFromDyn (Max x _) = [x]
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



