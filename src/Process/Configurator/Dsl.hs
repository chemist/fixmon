{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GADTs #-}
module Process.Configurator.Dsl
where

import           Types

import           Data.Map             (lookup)
import           Data.Text            hiding (empty, filter, foldl1, head, map, takeWhile)
-- import           Data.Text.Encoding   (encodeUtf8)
-- import qualified Prelude              as P
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Error as E
import Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>), pure)
import Data.Typeable
import Data.Attoparsec.Text
-- import Data.Text (pack)
import Data.Scientific (floatingOrInteger)

import           Prelude              hiding (lookup, takeWhile)

top :: Parser (TriggerRaw Bool)
top = logic <$> many1 (eitherP expr spliter)

logic :: [Either (TriggerRaw Bool) Lo] -> TriggerRaw Bool
logic (Left x:[]) = x
logic (Left x : Right y : xs) = loToLogic y x (logic xs)
logic _ = error "bad expression in trigger"

data Lo = A | O 

loToLogic :: Lo -> TriggerRaw Bool -> TriggerRaw Bool -> TriggerRaw Bool
loToLogic A = And 
loToLogic O = Or 

aP, oP, spliter :: Parser Lo
aP = skipSpace *> string "and" *> skipSpace *> pure A
oP = skipSpace *> string "or"  *> skipSpace *> pure O
spliter = aP <|> oP 

expr :: Parser (TriggerRaw Bool)
expr = Not <$> (string "not" *> skipSpace *> simpl) <|> simpl

simpl :: Parser (TriggerRaw Bool)
simpl = eql <|> boolP

eql :: Parser (TriggerRaw Bool)
eql = equalP <|> moreP <|> lessP
  where
  equalP = Equal <$> (nameP <* skipSpace <* string "equal") <*> (skipSpace *> returnP)
  moreP = More <$> (nameP <* skipSpace <* string "more") <*> (skipSpace *> returnP)
  lessP = Less <$> (nameP <* skipSpace <* string "less") <*> (skipSpace *> returnP)
 
boolP :: Parser (TriggerRaw Bool)
boolP = Bool <$> ((string "true" *> pure True) <|> (string "True" *> pure True) <|> (string "False" *> pure False) <|> (string "false" *> pure False))

nameP :: Parser (TriggerRaw Text)
nameP = Text <$> takeWhile1 (inClass "a-zA-Z0-9.")

returnP :: Parser Any
returnP = (Any <$> boolP) <|> num <|> (Any <$> nameP)


num :: Parser Any
num = do
    c <- scientific
    case floatingOrInteger c of
         Right x -> return $ Any . Int $ x
         Left x -> return $ Any . Double $ x


parseTrigger :: Text -> Either String (TriggerRaw Bool)
parseTrigger = parseOnly top 


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
