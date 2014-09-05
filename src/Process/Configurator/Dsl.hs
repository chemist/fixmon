{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Dsl
where

import           Types.Dynamic
import           Data.Text  (Text, pack) 
import           Control.Applicative hiding ((<|>), many)
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
dynP:: Parser (Exp Dyn)
dynP = try avg <|> try prev <|> try min <|> try max  <|> try lastF <|> try valP <|> try envVal

lastF :: Parser (Exp Dyn) 
lastF = Last <$> (string "last" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

envVal :: Parser (Exp Dyn) 
envVal = EnvVal <$> counterP 

avg :: Parser (Exp Dyn)
avg = Avg <$> (string "avg" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

prev :: Parser (Exp Dyn) 
prev = Prev <$> (string "prev" *> openQuote *> counterP <* closeQuote)

min :: Parser (Exp Dyn)
min = Min <$> (string "min" *> openQuote *> counterP <* comma ) <*> periodP <* closeQuote

max :: Parser (Exp Dyn)
max = Max <$> (string "max" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

valP :: Parser (Exp Dyn)
valP = Val <$> try ( num <|> quotedStr)

quotedStr :: Parser Dyn
quotedStr = do 
    void $ char '"'
    x <- many $ chars
    void $ char '"'
    return . toDyn . pack $ x
    where 
    chars = escaped <|> noneOf "\""
    escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']       --  "
    codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']        --  "

num :: Parser Dyn
num = do
    n <- number
    case n of
         Left i -> return $ toDyn i
         Right i -> return $ toDyn i


nodata :: Parser (Exp Bool)
nodata = NoData <$> (string "nodata" *> openQuote *> counterP <* comma) <*> periodP <* closeQuote

change :: Parser (Exp Bool)
change = Change <$> (string "change" *> openQuote *> counterP <* closeQuote)

------------------------------------------------------------------------------------------------------------------------------
-- Counter
counterP:: Parser Counter
counterP = spaces *> (Counter . pack <$> (many1 symbols )) <* spaces
   
symbols :: Parser Char
symbols = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-."

-- !, &&, ||
booleanP :: Parser (Exp Bool)
booleanP = undefined

notP :: Parser (Exp Bool)
notP = do
    void $ char '!' <* spaces
    b <- booleanP
    return $ Not b


-- >, <, >=, <=, !=, =
compareP :: Parser (Exp Bool)
compareP = undefined

------------------------------------------------------------------------------------------------------------------------------

period :: String -> Either ParseError Period
period = parse periodP "parse period"

periodP :: Parser Period
periodP = spaces *> choice [try time, try (Count <$> int)] <* spaces

time :: Parser Period
time = do
    i <- int
    q <- quant
    return $ Sec (i * q)

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
number = floatingOrInteger . read <$> (many1 (digit <|> char '.'))

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

spaces1 :: Parser ()
spaces1 = skipMany1 space
