{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
module GNS.Data where

import           Control.Exception
import           Data.Map            (Map, empty)
import qualified Data.Map            as Map
import           Data.Monoid         hiding (Any)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.String
import           Data.Text           hiding (empty)
import           Data.Typeable
import           System.Cron

import           Control.Monad.Error
import           Control.Monad.RWS.Strict   hiding (Any)

newtype Log = Log Text deriving (Show, Eq)

instance Monoid Log where
    mempty = Log ""
    Log x `mappend` Log y = Log $ x <> y

newtype Gns a = Gns {run:: ErrorT String (RWST StartOptions Log Monitoring IO) a} deriving 
  ( Monad
  , MonadIO
  , MonadError String
  , MonadReader StartOptions
  , MonadState Monitoring
  , MonadWriter Log
  , MonadRWS StartOptions Log Monitoring)

data StartOptions = StartOptions
  { config :: FilePath
  } deriving (Show, Eq)

flip' :: (ErrorT String (RWST StartOptions Log Monitoring IO) a -> b -> c -> d) -> b -> c -> Gns a -> d
flip' f a b c =  f (run c) a b

runGns :: StartOptions -> Monitoring -> Gns a -> IO (Either String a, Monitoring, Log)
runGns = flip' $ runRWST . runErrorT

newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord)
newtype CheckId = CheckId Int deriving (Show, Eq, Ord)

newtype Cron = Cron CronSchedule deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map Cron (Set CheckId)
 , _triggers  :: Map TriggerId Trigger
 , _checks    :: Map CheckId Check
 , _status    :: Map TriggerId Status
 , _raw       :: !GState
 } deriving Show

emptyMonitoring :: Monitoring
emptyMonitoring = Monitoring empty empty empty empty mempty

instance Ord Cron where
    compare x y = show x `compare` show y

instance Monoid Monitoring where
    mempty = Monitoring empty empty empty empty mempty
    Monitoring a b c d e `mappend` Monitoring a1 b1 c1 d1 e1 = Monitoring
      { _periodMap = Map.unionWith Set.union a a1
      , _triggers  = Map.union b b1
      , _checks    = Map.union c c1
      , _status    = Map.union d d1
      , _raw       = e `mappend` e1
      }

newtype CheckName = CheckName Text deriving (Eq, Ord)

instance Show CheckName where
    show (CheckName x) = show x

data Trigger = Trigger
  { _name        :: Text
  , _description :: Text
  , _check       :: CheckName
  , _result      :: TriggerFun
  } deriving Show

newtype Status = Status { unStatus :: Bool } deriving Show


instance Show TriggerFun where
     show _ = "trigger fun here"

data Group = Group
 { name     :: Text
 , hosts    :: [Hostname]
 , triggers :: [Text]
 } deriving Show

newtype Hostname = Hostname Text deriving (Eq, Show, Ord)

data GState = GState
  { hosts'    :: ! [Hostname]
  , groups'   :: ! [Group]
  , triggers' :: ! [Trigger]
  , checks'   :: ! [Check]
  } deriving (Show)

instance Monoid GState where
    mempty = GState [] [] [] []
    GState a b c d `mappend` GState a1 b1 c1 d1 = GState (a <> a1) (b <> b1) (c <> c1) (d <> d1)

----------------------------------------------------------------------------------------------------
-- check type
----------------------------------------------------------------------------------------------------

data TypeError = TypeError String deriving (Show, Typeable)

data PError = PError String deriving (Show, Typeable)

instance Exception PError
instance Error PError
instance Error TypeError
instance Exception TypeError

newtype Name = Name Text deriving (Show, Eq, Ord)

instance IsString Name where
    fromString x = Name . pack $ x

instance IsString CheckName where
    fromString x = CheckName . pack $ x

instance IsString Log where
    fromString x = Log . pack $ x


data Check = Check { _checkName :: CheckName
                   , _period    :: Cron
                   , _params    :: Map Text Text
                   } deriving (Show, Eq, Ord)

newtype TriggerFun = TriggerFun (Complex -> Status)

newtype Complex = Complex (Map Text Any) deriving Show

data Any where
  Any :: (Eq a, Ord a, Show a) => FFF a -> Any

instance Typeable Any where
    typeOf (Any (Int x)) = typeOf x
    typeOf (Any (Text x)) = typeOf x
    typeOf (Any (Bool x)) = typeOf x
    typeOf _ = throw $ TypeError "cant check Any type"

instance Eq Any where
    (==) a b | typeOf a == typeOf b = case (a,b) of
                      (Any (Bool x), Any (Bool y)) -> x == y
                      (Any (Int x), Any (Int y)) -> x == y
                      (Any (Text x), Any (Text y)) -> x == y
                      _ -> throw $ TypeError $ "unknown type " ++ show (typeOf a)
            | otherwise = throw $ TypeError $ "you try do " ++ show (typeOf a) ++ " == " ++ show (typeOf b)

instance Ord Any where
    compare a b | typeOf a == typeOf b =
                     case (a, b) of
                          (Any (Bool x), Any (Bool y)) -> compare x y
                          (Any (Int x), Any (Int y)) -> compare x y
                          (Any (Text x), Any (Text y)) -> compare x y
                          _ -> throw $ TypeError $ "unknown type " ++ show (typeOf a)
                | otherwise = throw $ TypeError $ "you try compare " ++ show (typeOf a) ++ " and " ++ show (typeOf b)




instance Show Any where
    show (Any x) = show x

data FFF a where
  Int :: Int -> FFF Int
  Bool :: Bool -> FFF Bool
  Text :: Text -> FFF Text

  Not :: FFF Bool -> FFF Bool
  Or :: FFF Bool -> FFF Bool -> FFF Bool
  And :: FFF Bool -> FFF Bool -> FFF Bool

  Less :: FFF Text -> Any -> FFF Bool
  More :: FFF Text -> Any -> FFF Bool
  Equal :: FFF Text -> Any -> FFF Bool


instance (Show a) => Show (FFF a) where
    show (Text x) = show x
    show (Bool x) = show x
    show (Int x ) = show x

    show (Not x ) = "not " ++ show x
    show (Or x y) = show x ++ " or " ++ show y
    show (And x y) = show x ++ "and " ++ show y

    show (Equal x y) = show x ++ " equal " ++ show y
    show (Less n r) = show n ++ " less " ++ show r
    show (More n r) = show n ++ " more " ++ show r


