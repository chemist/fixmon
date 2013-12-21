{-# LANGUAGE DeriveDataTypeable #-}
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
import           Control.Monad.RWS   hiding (Any)

newtype Log = Log Text

instance Monoid Log where
    mempty = Log ""
    Log x `mappend` Log y = Log $ x <> "\n" <> y

newtype Gns a = Gns {run:: ErrorT Text (RWST GState Log Monitoring IO) a}

flip' :: (ErrorT Text (RWST GState Log Monitoring IO) a -> b -> c -> d) -> b -> c -> Gns a -> d
flip' f a b c =  f (run c) a b

runGns :: GState -> Monitoring -> Gns a -> IO (Either Text a, Monitoring, Log)
runGns = flip' $ runRWST . runErrorT

newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord)
newtype CheckId = CheckId Int deriving (Show, Eq, Ord)

data Monitoring = Monitoring
 { _periodMap :: Map CronSchedule (Set CheckId)
 , _triggers  :: Map TriggerId Trigger
 , _checks    :: Map CheckId Check
 , _status    :: Map TriggerId Status
 } deriving Show

emptyMonitoring :: Monitoring
emptyMonitoring = Monitoring empty empty empty empty

newtype CheckName = CheckName Text

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

instance Exception TypeError

newtype Name = Name Text deriving (Show, Eq, Ord)

instance IsString Name where
    fromString x = Name . pack $ x


data Check = Check { _checkName :: CheckName
                   , _period    :: CronSchedule
                   , _params    :: Map Text Text
                   } deriving Show

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
            | otherwise = throw $ TypeError $ "you try do " ++ show (typeOf a) ++ " == " ++ show (typeOf b)

instance Ord Any where
    compare a b | typeOf a == typeOf b =
                     case (a, b) of
                          (Any (Bool x), Any (Bool y)) -> compare x y
                          (Any (Int x), Any (Int y)) -> compare x y
                          (Any (Text x), Any (Text y)) -> compare x y
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


