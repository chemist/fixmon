{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GADTs #-}
module Process.Configurator.Dsl
where

import           Types

import qualified Data.ListLike        as LL
import           Data.Map             (lookup)
import           Data.Text            hiding (empty, filter, foldl1, head, map)
import           Data.Text.Encoding   (encodeUtf8)
import           Prelude              hiding (lookup)
import qualified Prelude              as P
import           Text.Peggy           hiding (And, Not)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Error as E
import Control.Applicative ((<$>), (<*>))
import Data.Typeable



[peggy|

top :: TriggerRaw Bool = expr

expr :: TriggerRaw Bool
  = expr "and" simpl { And $1 $2 }
  / expr "or"  simpl { Or  $1 $2 }
  / "not" expr { Not $1 }
  / simpl { $1 }

simpl :: TriggerRaw Bool 
  = pName "equal" pReturn { Equal $1 $2 }
  / pName "more"  pReturn { More  $1 $2 }
  / pName "less"  pReturn { Less  $1 $2 }
  / pBool

pName :: TriggerRaw Text
  =  [a-zA-Z0-9]+ { Text (pack $1) }

pBool :: TriggerRaw Bool
  = "false" { Bool False }
  / "False" { Bool False }
  / "true"  { Bool True }
  / "True"  { Bool True }

pReturn :: Any
  = "true" { Any $ Bool True }
  / "True" { Any $ Bool True }
  / "false" { Any $ Bool False }
  / "False" { Any $ Bool False }
  / num     { Any $ Int $1 }
  / pName   { Any $ $1 }

num ::: Int
  = [0-9]+ { read $1 }
  / [-] [0-9]+ { read ($1:$2) }

|]

parseTrigger :: Text -> Either ParseError (TriggerRaw Bool)
parseTrigger x = parseString top "<stdin>" $ LL.CS $ encodeUtf8 x  


data Env = Env 

type Eval a = R.ReaderT Env (E.ErrorT String IO) a


eval :: Env -> Complex -> TriggerRaw a -> IO (Either String Bool)
eval env c e = E.runErrorT (R.runReaderT (fun c e) env)

fun :: Complex -> TriggerRaw a -> Eval Bool
fun (Complex c) (Less (Text x) y)  = maybe (fail "bad check describe") (less y) (lookup x c)
fun (Complex c) (More (Text x) y)  = maybe (fail "bad check describe") (more y) (lookup x c)
fun (Complex c) (Equal (Text x) y) = maybe (fail "bad check describe") (equal y) (lookup x c) 
fun (Complex c) (Not x)            = not <$> fun (Complex c) x
fun (Complex c) (And x y)          = (&&) <$> fun (Complex c) x <*> fun (Complex c) y
fun (Complex c) (Or x y)           = (||) <$> fun (Complex c) x <*> fun (Complex c) y
fun _  _                           = return False

class Logic a where
    equal :: a -> a -> Eval Bool
    more :: a -> a -> Eval Bool
    less :: a -> a -> Eval Bool

instance Logic Any where
    equal a b | typeOf a == typeOf b = case (a,b) of
                      (Any (Bool x), Any (Bool y)) -> return $ x == y
                      (Any (Int x), Any (Int y)) -> return $ x == y
                      (Any (Text x), Any (Text y)) -> return $ x == y
                      _ -> fail $ "unknown type " ++ show (typeOf a)
            | otherwise = fail $ "you try do " ++ show (typeOf a) ++ " == " ++ show (typeOf b)
    more a b | typeOf a == typeOf b = case (a, b) of
                      (Any (Bool x), Any (Bool y)) -> return $ x > y
                      (Any (Int x), Any (Int y)) -> return $ x > y
                      (Any (Text x), Any (Text y)) -> return $ x > y
                      _ -> fail $ "unknown type " ++ show (typeOf a)
             | otherwise = fail $ "you try do " ++ show (typeOf a) ++ " > " ++ show (typeOf b)
    less a b | typeOf a == typeOf b = case (a, b) of
                      (Any (Bool x), Any (Bool y)) -> return $ x < y
                      (Any (Int x), Any (Int y)) -> return $ x < y
                      (Any (Text x), Any (Text y)) -> return $ x < y
                      _ -> fail $ "unknown type " ++ show (typeOf a)
             | otherwise = fail $ "you try do " ++ show (typeOf a) ++ " < " ++ show (typeOf b)

-- | Examples
-- 
-- >>> let a = Text $ pack "key"
-- >>> let b = Any $ Bool True
-- >>> let c = Equal a b
-- >>> let m = Complex $ Map.fromList [(pack "key", Any (Bool True)), (pack "int", Any (Int 3))]
-- >>> let m1 = Complex $ Map.fromList [(pack "bool", Any (Bool True)), (pack "key", Any (Int 3))]
-- 
-- >>> eval c m
-- Status {unStatus = True}
-- >>> eval c m1
-- Status {unStatus = *** Exception: TypeError "you try do Int == Bool"
