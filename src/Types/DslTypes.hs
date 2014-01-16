{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Types.DslTypes
( TriggerRaw(..), Any(..) )
where

import           Control.Applicative ((<$>), (<*>))
import           Control.Exception   (Exception, throw)
import           Control.Monad.Error (Error)
import           Data.Binary         (Binary, get, getWord8, put, putWord8)
import           Data.Text           (Text)
import           Data.Text.Binary    ()
import           Data.Typeable       (Typeable, Typeable1, typeOf)

data TypeError = TypeError String deriving (Show, Typeable)
instance Error TypeError
instance Exception TypeError


data Any where
  Any :: (Eq a, Ord a, Show a, Binary a) => TriggerRaw a -> Any

deriving instance Typeable Any

instance Show Any where
    show (Any x) = show x

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



