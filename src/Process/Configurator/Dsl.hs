{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Process.Configurator.Dsl
where

import           Types
import           Data.Text            hiding (empty, filter, foldl1, head, map,
                                       takeWhile)
import           Control.Applicative  (pure, (*>), (<$>), (<*), (<*>), (<|>))
import           Data.Attoparsec.Text
import           Data.Char            (isSpace)
import           Data.Scientific      (floatingOrInteger)

import           Prelude              hiding (lookup, takeWhile)

top :: Parser (Exp Bool)
top = logic <$> many1 (eitherP expr spliter)

logic :: [Either (Exp Bool) Lo] -> Exp Bool
logic (Left x:[]) = x
logic (Left x : Right y : xs) = loToLogic y x (logic xs)
logic _ = error "bad expression in trigger"

data Lo = A | O

loToLogic :: Lo -> Exp Bool -> Exp Bool -> Exp Bool
loToLogic A = And
loToLogic O = Or

aP, oP, spliter :: Parser Lo
aP = string "and" *> sp *> pure A
oP = string "or"  *> sp *> pure O
spliter = aP <|> oP

expr :: Parser (Exp Bool)
expr = Not <$> (string "not" *> sp *> simpl) <|> simpl

simpl :: Parser (Exp Bool)
simpl = eql 

eql :: Parser (Exp Bool)
eql = equalP <|> moreP <|> lessP
  where
  equalP = Equal <$> (nameP <* string "equal") <*> (sp *> returnP)
  moreP = More <$> (nameP <* string "more") <*> (sp *> returnP)
  lessP = Less <$> (nameP <* string "less") <*> (sp *> returnP)

boolP :: Parser (Exp Dyn)
boolP = Val . toDyn <$> ((string "true" *> sp *> pure True) <|> (string "True" *> sp *> pure True) <|> (string "False" *> sp *> pure False) <|> (string "false" *> sp *> pure False))

nameP :: Parser (Exp Dyn)
nameP = EnvVal . Counter <$> takeWhile1 (inClass "a-zA-Z0-9.-") <* sp

returnP :: Parser (Exp Dyn)
returnP = boolP <|> num <|> nameP


num :: Parser (Exp Dyn)
num = do
    c <- scientific <* sp
    case floatingOrInteger c of
         Right x -> return $ Val . toDyn $ (x :: Int)
         Left x -> return $ Val . toDyn $ (x :: Double)

sp :: Parser ()
sp = takeWhile1 isSpace *> pure () <|> takeWhile isSpace *> endOfInput *> pure ()

parseTrigger :: Text -> Either String (Exp Bool)
parseTrigger = parseOnly top


data Env = Env

data ConfigError = ConfigError String

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
