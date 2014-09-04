{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Dsl
where

import           Types.Dynamic
import           Data.Text  (Text, pack) 
import           Control.Applicative hiding ((<|>))
import           Data.Either
-- import           Data.Char            (isSpace)
import           Data.Scientific      (floatingOrInteger)
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Control.Monad (void)
import Prelude hiding (min, max, last)

parseTrigger :: Text -> Either String (Exp Bool)
parseTrigger = undefined

{-| functions:
- data Fun = ChangeFun Counter
-          | LastFun Counter Period
-          | AvgFun Counter Period
-          | PrevFun Counter
-          | MinFun Counter Period
-          | MaxFun Counter Period
-          | NoDataFun Counter Period
-}
-- change, last, avg, prev, min, max, nodata
funP:: Parser Fun
funP = lastS <|> change <|> avg <|> prev <|> min <|> max <|> nodata <|> lastF 

change :: Parser Fun
change = ChangeFun <$> (string "change" *> openQuote *> counterP <* closeQuote)

lastF :: Parser Fun 
lastF = LastFun <$> (string "last" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

lastS :: Parser Fun 
lastS = LastFun <$> counterP <*> pure (Count 0)

avg :: Parser Fun
avg = AvgFun <$> (string "avg" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

prev :: Parser Fun 
prev = PrevFun <$> (string "prev" *> openQuote *> counterP <* closeQuote)

min :: Parser Fun
min = MinFun <$> (string "min" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

max :: Parser Fun
max = MinFun <$> (string "max" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

nodata :: Parser Fun
nodata = NoDataFun <$> (string "nodata" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

fun :: String -> Either ParseError Fun
fun = parse funP "fun parse"

------------------------------------------------------------------------------------------------------------------------------
-- Counter
counterP:: Parser Counter
counterP = spaces *> (Counter . pack <$> (manyTill anyChar (comma <|> spaces))) <* spaces
   

-- !, &&, ||
booleanP :: Parser (Exp Bool)
booleanP= undefined

-- >, <, >=, <=, !=, =
compareP :: Parser (Exp Bool)
compareP = undefined

------------------------------------------------------------------------------------------------------------------------------
periodP :: Parser Period
periodP = spaces *> (time <|> (Count <$> int)) <* spaces

time :: Parser Period
time = Sec <$> ((*) <$> int <*> quant)

quant :: Parser Int 
quant = (string "s" *> pure 1) <|> 
        (string "m" *> pure 60) <|> 
        (string "h" *> pure 3600) <|> 
        (string "d" *> pure (3600 * 24)) <|> 
        (string "w" *> pure (3600 * 24 * 7)) <|>
        (string "m" *> pure (3600 * 24 * 30)) <|>
        (string "y" *> pure (3600 * 24 * 365))

------------------------------------------------------------------------------------------------------------------------------
openQuote :: Parser ()
openQuote = void $ char '('

closeQuote :: Parser ()
closeQuote = void $ char ')'

comma :: Parser ()
comma = void $ char ','

number :: Parser (Either Double Int)
number = floatingOrInteger . read <$> many1 digit

left :: Either a b -> a
left (Left a) = a
left _ = error "must be Double"

right :: Either a b -> b
right (Right a) = a
right _ = error "must be Int"

int :: Parser Int
int = right <$> number

double :: Parser Double
double = left <$> number

spaces :: Parser ()
spaces = skipMany space
