{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE FlexibleInstances          #-}
module GNS.Data where

import           Control.Exception
import           Data.Map                 (Map, empty)
import           Data.Monoid              hiding (Any)
import           Data.Set                 (Set)
import           Data.String
import           Data.Text                hiding (empty)
import           Data.Typeable
import           System.Cron

import           Control.Applicative      ((<$>), (<*>))
import           Control.Monad.Error
import           Control.Monad.RWS.Strict hiding (Any, get, put)
import           Data.Binary
import           Data.Text.Binary         ()

newtype Log = Log Text deriving (Show, Eq, Typeable)
newtype TriggerId = TriggerId Int deriving (Show, Eq, Ord, Binary, Read)
newtype CheckId = CheckId Int deriving (Show, Eq, Ord, Binary, Read)
newtype Cron = Cron CronSchedule deriving (Show, Eq, Typeable)
newtype GroupId = GroupId Int deriving (Show, Eq, Ord, Binary)
newtype HostId = HostId Int deriving (Show, Eq, Ord, Binary, Typeable, Read)
newtype CheckName = CheckName Text deriving (Eq, Ord, Binary)
newtype Status = Status { unStatus :: Bool } deriving (Show, Eq, Ord, Binary)
newtype TriggerName = TriggerName Text deriving (Eq, Show, Ord, Binary)
newtype GroupName = GroupName Text deriving (Eq, Show, Ord, Binary)
newtype Hostname = Hostname Text deriving (Eq, Show, Ord, Binary, Typeable)
newtype Name = Name Text deriving (Show, Eq, Ord, Binary)
newtype CheckHost = CheckHost (HostId, CheckId) deriving (Show, Eq, Ord, Binary, Typeable)
newtype TriggerHost = TriggerHost (HostId, TriggerId) deriving (Show, Eq, Ord)

newtype Complex = Complex (Map Text Any) deriving Show
newtype TriggerFun = TriggerFun (Complex -> Status)

newtype Gns a = Gns {run:: ErrorT String (RWST StartOptions Log Monitoring IO) a} deriving
  ( Monad
  , MonadIO
  , Functor
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
  , _result      :: TriggerRaw Bool
  } deriving Show


instance Binary Trigger where
    put (Trigger n d c r) = put n >> put d >> put c >> put r
    get = Trigger <$> get <*> get <*> get <*> get

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
                   } deriving (Show, Eq, Ord, Typeable)

instance Binary Check where
    put (Check a b c) = put a >> put b >> put c
    get = Check <$> get <*> get <*> get

data Any where
  Any :: (Eq a, Ord a, Show a, Binary a) => TriggerRaw a -> Any

instance Binary Any where
    put (Any (Int x)) = putWord8 0 >> put x
    put (Any (Bool x)) = putWord8 1 >> put x
    put (Any (Text x)) = putWord8 2 >> put x
    put _ = fail "you dont need this"
    get = do mark <- getWord8
             case mark of
                  0 -> get >>= \x -> return $ Any (Int x)
                  1 -> get >>= \x -> return $ Any (Bool x)
                  2 -> get >>= \x -> return $ Any (Text x)
                  _ -> fail "unknown mark in binary any"

data TriggerRaw a where
  Int :: Int -> TriggerRaw Int
  Bool :: Bool -> TriggerRaw Bool
  Text :: Text -> TriggerRaw Text

  Not :: TriggerRaw Bool -> TriggerRaw Bool
  Or :: TriggerRaw Bool -> TriggerRaw Bool -> TriggerRaw Bool
  And :: TriggerRaw Bool -> TriggerRaw Bool -> TriggerRaw Bool

  Less :: TriggerRaw Text -> Any -> TriggerRaw Bool
  More :: TriggerRaw Text -> Any -> TriggerRaw Bool
  Equal :: TriggerRaw Text -> Any -> TriggerRaw Bool

deriving instance Typeable1 TriggerRaw 

instance Binary (TriggerRaw Int) where
    put (Int x)  = putWord8 0 >> put x
    get = getWord8 >>= \x -> case x of
                                    0 -> Int <$> get 
                                    _ -> fail "bad mark in triggerRaw int"

instance Binary (TriggerRaw Text) where
    put (Text x) = putWord8 2 >> put x
    get = getWord8 >>= \x -> case x of
                                    2 -> Text <$> get 
                                    _ -> fail "bad mark in triggerRaw text"

instance Binary (TriggerRaw Bool) where
    put (Bool x) = putWord8 1 >> put x
    put (Not x) = putWord8 3 >> put x
    put (Or x y) = putWord8 4 >> put x >> put y
    put (And x y) = putWord8 5 >> put x >> put y
    put (Less x y) = putWord8 6 >> put x >> put y
    put (More x y) = putWord8 7 >> put x >> put y
    put (Equal x y) = putWord8 8 >> put x >> put y
    get = do mark <- getWord8
             case mark of
                  1 -> Bool <$> get 
                  3 -> Not <$> get
                  4 -> Or <$> get <*> get
                  5 -> And <$> get <*> get
                  6 -> Less <$> get <*> get
                  7 -> More <$> get <*> get
                  8 -> Equal <$> get <*> get
                  _ -> fail "bad mark in triggerRaw bool"

instance Show CheckName where
    show (CheckName x) = show x

instance Show TriggerFun where
     show _ = "trigger fun here"

instance Show Any where
    show (Any x) = show x

instance (Show a) => Show (TriggerRaw a) where
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

deriving instance Typeable Any

{--
instance Typeable Any where
    typeOf (Any (Int x)) = typeOf x
    typeOf (Any (Text x)) = typeOf x
    typeOf (Any (Bool x)) = typeOf x
    typeOf _ = throw $ TypeError "cant check Any type"
    --}

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


instance Binary CronField where
    put (Star) = putWord8 0
    put (SpecificField x) = putWord8 1 >> put x
    put (RangeField x y) = putWord8 2 >> put x >> put y
    put (ListField x) = putWord8 3 >> put x
    put (StepField x y) = putWord8 4 >> put x >> put y

    get = do x <- getWord8
             case x of
                  0 -> return Star
                  1 -> SpecificField <$> get
                  2 -> RangeField <$> get <*> get
                  3 -> ListField <$> get
                  4 -> StepField <$> get <*> get
                  _ -> fail "bad binary"

instance Binary Cron where
    put (Cron (CronSchedule (Minutes a) (Hours b) (DaysOfMonth c) (Months d) (DaysOfWeek e))) = put a >> put b >> put c >> put d >> put e
    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        return $ Cron (CronSchedule (Minutes a) (Hours b) (DaysOfMonth c) (Months d) (DaysOfWeek e))


