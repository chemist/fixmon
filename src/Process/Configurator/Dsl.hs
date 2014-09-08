{-# LANGUAGE OverloadedStrings #-}
module Process.Configurator.Dsl
where

import           Types.Dynamic
import           Data.Text  (Text, pack) 
import           Control.Applicative hiding ((<|>), many)
import           Data.Either
-- import           Data.Char            (isSpace)
import           Data.Monoid ((<>))
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Control.Monad (void)
import Prelude hiding (min, max, last)

parseTrigger :: Text -> Either String (Exp Bool)
parseTrigger = undefined

-- change, last, avg, prev, min, max, nodata
dynP:: Parser (Exp Dyn)
dynP = try avg <|> try prev <|> try min <|> try max  <|> try lastF <|> try valP <|> envVal

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
valP = Val <$> try ( periodDyn <|> num <|> quotedStr)

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

periodDyn :: Parser Dyn
periodDyn = do
    p <- periodP
    return $ toDyn (un p)


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
booleanP = try nodata <|> try change <|> try compareP 

simpleTriggerP :: Parser (Exp Bool)
simpleTriggerP = booleanP

data Logic = NotToken | AndToken | OrToken | OpenQuote | CloseQuote | Simple (Exp Bool) deriving (Show, Eq)

notToken, andToken, orToken, open, close, simpleToken, tok :: Parser Logic
notToken = spaces *> char '!' *> spaces *> pure NotToken
andToken = spaces *> char '&' *> char '&' *> spaces *> pure AndToken
orToken = spaces *> char '|' *> char '|' *> spaces *> pure OrToken
open = openQuote *> pure OpenQuote
close = closeQuote *> pure CloseQuote
simpleToken = Simple <$> ( spaces *> simpleTriggerP <* spaces)
tok = try open <|> try close <|> try notToken <|> try andToken <|> try orToken <|> simpleToken

tokenizer :: Parser [Logic]
tokenizer = many1 tok <* eof


etrigger :: [Logic] -> Either String ETrigger
etrigger xs = undefined

-- >, <, >=, <=, !=, =
compareP :: Parser (Exp Bool)
compareP = try moreP <|> try lessP <|> try equalP

moreP :: Parser (Exp Bool)
moreP = More <$> dynP <* moreP' <*> dynP
    where
    moreP' = void $ spaces *> char '>' <* spaces

lessP :: Parser (Exp Bool)
lessP = Less <$> dynP <* lessP' <*> dynP
    where
    lessP' = void $ spaces *> char '<' <* spaces

equalP :: Parser (Exp Bool)
equalP = Equal <$> dynP <* equalP' <*> dynP
    where
    equalP' = void $ spaces *> char '=' <* spaces

------------------------------------------------------------------------------------------------------------------------------

period :: String -> Either ParseError (Period Int)
period = parse periodP "parse period"

periodP :: Parser (Period Int)
periodP = spaces *> choice [try time, try (Count <$> int)] <* spaces

time :: Parser (Period Int)
time = do
    i <- int
    q <- quant
    return $ (*i) <$> q

------------------------------------------------------------------------------------------------------------------------------
openQuote :: Parser ()
openQuote = void $ char '('

closeQuote :: Parser ()
closeQuote = void $ char ')'

comma :: Parser ()
comma = void $ char ','

number :: Parser (Either Double Int)
number = do
    a <- optionMaybe (many1 digit)
    p <- optionMaybe (char '.')
    b <- optionMaybe (many1 digit)
    case (a, p, b) of
         (Nothing, Just _ , Just x ) -> return (Left $ read $ "0." <> x)
         (Just x , Nothing, _      ) -> return (Right (read x))
         (Just x , Just _ , Nothing) -> return (Left $ read x )
         (Just x , Just _ , Just y ) -> return (Left $ read $ x <> "." <> y)
         _ -> fail "bad number"

quant :: Parser (Period Int)
quant = try (string "mk") *> (pure $ MicroSec 1 )
    <|> try (string "ms") *> (pure $ MicroSec 1000)
    <|> try (char 's'   ) *> (pure $ MicroSec 1000000)
    <|> try (char 'm'   ) *> (pure $ MicroSec ( 60 * 1000000))
    <|> try (char 'h'   ) *> (pure $ MicroSec ( 60 * 60 * 1000000))
    <|> try (char 'D'   ) *> (pure $ MicroSec ( 24 * 60 * 60 * 1000000))
    <|> try (char 'W'   ) *> (pure $ MicroSec ( 7 * 24 * 60 * 60 * 1000000))
    <|> try (char 'M'   ) *> (pure $ MicroSec ( 30 * 24 * 60 * 60 * 1000000))
    <|> try (char 'Y'   ) *> (pure $ MicroSec ( 365 * 24 * 60 * 60 * 1000000))
    <|> try (char 'B'   ) *> (pure $ Byte 1)
    <|> try (string "KB") *> (pure $ Byte 1024)
    <|> try (string "MB") *> (pure $ Byte ( 1024 * 1024))
    <|> try (string "GB") *> (pure $ Byte ( 1024 * 1024 * 1024))
    <|> try (string "TB") *> (pure $ Byte ( 1024 * 1024 * 1024 * 1024))

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
