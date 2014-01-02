{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
module GNS.Data where

import           Control.Exception
import           Data.Map            (Map, empty)
import           Data.Monoid         hiding (Any)
import           Data.Set            (Set)
import           Data.String
import           Data.Text           hiding (empty)
import           Data.Typeable
import           System.Cron

import           Control.Monad.Error
import           Control.Monad.RWS.Strict   hiding (Any)

newtype Log = Log Text deriving (Show, Eq)
newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord)
newtype CheckId = CheckId Int deriving (Show, Eq, Ord)
newtype Cron = Cron CronSchedule deriving (Show, Eq)
newtype GroupId = GroupId Int deriving (Show, Eq, Ord)
newtype HostId = HostId Int deriving (Show, Eq, Ord)
newtype CheckName = CheckName Text deriving (Eq, Ord)
newtype Status = Status { unStatus :: Bool } deriving Show
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord)
newtype Name = Name Text deriving (Show, Eq, Ord)
newtype CheckHost = CheckHost (HostId, CheckId) deriving (Show, Eq, Ord)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)

newtype Complex = Complex (Map Text Any) deriving Show
newtype TriggerFun = TriggerFun (Complex -> Status)

newtype Gns a = Gns {run:: ErrorT String (RWST StartOptions Log Monitoring IO) a} deriving 
  ( Monad
  , MonadIO
  , MonadError String
  , MonadReader StartOptions
  , MonadState Monitoring
  , MonadWriter Log
  , MonadRWS StartOptions Log Monitoring
  )

data StartOptions = StartOptions
  { config :: FilePath
  } deriving (Show, Eq)

data Monitoring = Monitoring
 { _periodMap :: Map Cron (Set CheckHost)
 , _checkHost :: Map CheckHost (Set TriggerId)
 , _hosts     :: Map HostId Hostname
 , _groups    :: [Group]
 , _triggers  :: Map TriggerId Trigger
 , _checks    :: Map CheckId Check
 , _status    :: Map TriggerHost Status
 } deriving Show

data Trigger = Trigger
  { _name        :: TriggerName
  , _description :: Text
  , _check       :: CheckId
  , _result      :: TriggerFun
  } deriving Show

data Group = Group
 { name     :: GroupName
 , hosts    :: Set HostId
 , triggers :: Set TriggerId
 , checks   :: Set CheckId
 } deriving Show

data TypeError = TypeError String deriving (Show, Typeable)
data PError = PError String deriving (Show, Typeable)

data Check = Check { _checkName :: CheckName
                   , _period    :: Cron
                   , _params    :: Map Text Text
                   } deriving (Show, Eq, Ord)

data Any where
  Any :: (Eq a, Ord a, Show a) => FFF a -> Any

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


instance Show CheckName where
    show (CheckName x) = show x

instance Show TriggerFun where
     show _ = "trigger fun here"

instance Show Any where
    show (Any x) = show x

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


instance IsString Name where
    fromString x = Name . pack $ x

instance IsString TriggerName where
    fromString x = TriggerName . pack $ x

instance IsString GroupName where
    fromString x = GroupName . pack $ x

instance IsString CheckName where
    fromString x = CheckName . pack $ x

instance IsString Log where
    fromString x = Log . pack $ x


instance Monoid Log where
    mempty = Log ""
    Log x `mappend` Log y = Log $ x <> y

instance Exception PError
instance Error PError
instance Error TypeError
instance Exception TypeError

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

instance Ord Cron where
    compare x y = show x `compare` show y

----------------------------------------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------------------------------------

flip' :: (ErrorT String (RWST StartOptions Log Monitoring IO) a -> b -> c -> d) -> b -> c -> Gns a -> d
flip' f a b c =  f (run c) a b

runGns :: StartOptions -> Monitoring -> Gns a -> IO (Either String a, Monitoring, Log)
runGns = flip' $ runRWST . runErrorT

emptyMonitoring :: Monitoring
emptyMonitoring = Monitoring empty empty empty mempty empty empty mempty



