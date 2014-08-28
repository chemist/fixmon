{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Process.Configurator.Dsl
where

import           Types

import           Data.Map.Strict             (lookup)
import           Data.Text            hiding (empty, filter, foldl1, head, map,
                                       takeWhile)
-- import           Data.Text.Encoding   (encodeUtf8)
-- import qualified Prelude              as P
import           Control.Applicative  (pure, (*>), (<$>), (<*), (<*>), (<|>))
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import           Data.Attoparsec.Text
import           Data.Char            (isSpace)
import           Data.Typeable
-- import Data.Text (pack)
import           Data.Scientific      (floatingOrInteger)

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
aP = string "and" *> sp *> pure A
oP = string "or"  *> sp *> pure O
spliter = aP <|> oP

expr :: Parser (TriggerRaw Bool)
expr = Not <$> (string "not" *> sp *> simpl) <|> simpl

simpl :: Parser (TriggerRaw Bool)
simpl = eql <|> boolP

eql :: Parser (TriggerRaw Bool)
eql = equalP <|> moreP <|> lessP
  where
  equalP = Equal <$> (nameP <* string "equal") <*> (sp *> returnP)
  moreP = More <$> (nameP <* string "more") <*> (sp *> returnP)
  lessP = Less <$> (nameP <* string "less") <*> (sp *> returnP)

boolP :: Parser (TriggerRaw Bool)
boolP = Bool <$> ((string "true" *> sp *> pure True) <|> (string "True" *> sp *> pure True) <|> (string "False" *> sp *> pure False) <|> (string "false" *> sp *> pure False))

nameP :: Parser (TriggerRaw Text)
nameP = Text <$> takeWhile1 (inClass "a-zA-Z0-9.-") <* sp

returnP :: Parser Any
returnP = (Any <$> boolP) <|> num <|> (Any <$> nameP)


num :: Parser Any
num = do
    c <- scientific <* sp
    case floatingOrInteger c of
         Right x -> return $ Any . Int $ x
         Left x -> return $ Any . Double $ x

sp :: Parser ()
sp = takeWhile1 isSpace *> pure () <|> takeWhile isSpace *> endOfInput *> pure ()

parseTrigger :: Text -> Either String (TriggerRaw Bool)
parseTrigger = parseOnly top


data Env = Env

data ConfigError = ConfigError String

type Eval a = R.ReaderT Env (E.ExceptT String IO) a


eval :: Env -> Complex -> TriggerRaw a -> IO (Either String Bool)
eval env c e = E.runExceptT (R.runReaderT (fun c e) env)

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
