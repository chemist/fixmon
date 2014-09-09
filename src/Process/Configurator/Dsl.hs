{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Process.Configurator.Dsl
where

import           Control.Applicative                    hiding ((<|>))
import           Control.Monad
import           Data.Monoid                            ((<>))
import           Data.Text                              (Text, pack)
import           Prelude                                hiding (max, min)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
import           Types.Dynamic

parseTrigger :: Text -> Either String Exp
parseTrigger = undefined

languageDef :: LanguageDef st
languageDef =
  emptyDef { Token.commentStart = ""
           , Token.commentEnd   = ""
           , Token.commentLine  = ""
           , Token.identStart   = letter
           , Token.identLetter  = letter <|> char '.'
           , Token.reservedNames = [ "last"
                                   , "avg"
                                   , "min"
                                   , "max"
                                   , "nodata"
                                   , "change"
                                   , "prev"
                                   ]
           , Token.reservedOpNames = [ "&&", "||", "!", "=", ">", "<" ]
           }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef

identifier :: Parser Text
identifier = pack <$> Token.identifier lexer -- parses an identifier

reserved :: String -> Parser ()
reserved   = Token.reserved   lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

parens :: Parser Exp -> Parser Exp
parens     = Token.parens     lexer -- parses surrounding parenthesis:

semi :: Parser String
semi       = Token.semi       lexer -- parses a semicolon

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

stringLiteral :: Parser Text
stringLiteral = pack <$> Token.stringLiteral lexer

expr :: Parser Exp
expr = whiteSpace >> topLevel

topLevel :: Parser Exp
topLevel = buildExpressionParser bOperators bTerm

bOperators :: [[Operator Char () Exp]]
bOperators = [ [Prefix (try $ whiteSpace >> reservedOp "!" >> return Not) ]
             , [Infix  (try $ whiteSpace >> reservedOp "&&" >> return And) AssocLeft]
             , [Infix  (try $ whiteSpace >> reservedOp "||" >> return Or) AssocLeft]
             ]

bTerm :: Parser Exp
bTerm = parens topLevel <|> (middleLevel <|> nodata <|> change)

middleLevel :: Parser Exp
middleLevel = do
  a <- funs
  f <- relation
  b <- funs
  return $ f a b

relation :: Parser (DynExp -> DynExp -> Exp)
relation = (try $ whiteSpace *> reservedOp "=" *> return Equal)
       <|> (try $ whiteSpace *> reservedOp ">" *> return More)
       <|> (try $ whiteSpace *> reservedOp "<" *> return Less)



funs :: Parser DynExp
funs = (fun "min" Min) <|> (fun "max" Max) <|> (fun "last" Last) <|> (fun "avg" Avg) <|> prev <|> envval

fun :: String -> (Counter -> Period Int -> DynExp) -> Parser DynExp
fun i f = f <$> (reserved i *> char '(' *> (Counter <$> identifier) <* char ',') <*> periodP <* char ')'

prev :: Parser DynExp
prev = Prev <$> (reserved "prev" *> char '(' *> (Counter <$> identifier) <* char ')')

envval :: Parser DynExp
envval = EnvVal <$> Counter <$> identifier

val :: Parser DynExp
val = Val <$> (try num <|> try periodDyn <|> str)

str :: Parser Dyn
str = toDyn <$> stringLiteral

periodDyn ::Parser Dyn
periodDyn = toDyn . un <$> periodP

num :: Parser Dyn
num = do
    n <- number
    case n of
      Left i -> return $ toDyn i
      Right i -> return $ toDyn i

number :: Parser (Either Double Int)
number = do
    a <- try $ optionMaybe (many1 digit)
    p <- try $ optionMaybe (char '.')
    b <- try $ optionMaybe (many1 digit)
    case (a, p, b) of
      (Nothing, Just _ , Just x ) -> return (Left $ read $ "0." <> x)
      (Just x , Nothing, _      ) -> return (Right (read x))
      (Just x , Just _ , Nothing) -> return (Left $ read x )
      (Just x , Just _ , Just y ) -> return (Left $ read $ x <> "." <> y)
      _ -> fail "bad number"

int :: Parser Int
int = right <$> number

right :: Either a b -> b
right (Right a) = a
right _ = error "must be Int"

change :: Parser Exp
change = Change <$> (reserved "change" *> char '(' *> (Counter <$> identifier) <* char ')')

nodata :: Parser Exp
nodata = NoData <$> (reserved "nodata" *> char '(' *> (Counter <$> identifier) <* char ',') <*> periodP <* char ')'

periodP :: Parser (Period Int)
periodP = spaces *> choice [try time, try (Count <$> int)] <* spaces

time :: Parser (Period Int)
time = do
    i <- int
    q <- quant
    return $ (*i) <$> q

quant :: Parser (Period Int)
quant = try (string "mk") *> (pure $ MicroSec 1 )
    <|> try (string "ms") *> (pure $ MicroSec 1000)
    <|> try (char 's'   ) *> (pure $ MicroSec 1000000)
    <|> try (char 'm'   ) *> (pure $ MicroSec ( 60 * 1000000))
    <|> try (char 'h'   ) *> (pure $ MicroSec ( 60 * 60 * 1000000))
    <|> try (char 'D'   ) *> (pure $ MicroSec ( 24 * 60 * 60 * 1000000))
    <|> try (char 'W'   ) *> (pure $ MicroSec ( 7 * 24 * 60 * 60 * 1000000))
    <|> try (string "MB") *> (pure $ Byte ( 1024 * 1024))
    <|> try (char 'M'   ) *> (pure $ MicroSec ( 30 * 24 * 60 * 60 * 1000000))
    <|> try (char 'Y'   ) *> (pure $ MicroSec ( 365 * 24 * 60 * 60 * 1000000))
    <|> try (char 'B'   ) *> (pure $ Byte 1)
    <|> try (string "KB") *> (pure $ Byte 1024)
    <|> try (string "GB") *> (pure $ Byte ( 1024 * 1024 * 1024))
    <|> try (string "TB") *> (pure $ Byte ( 1024 * 1024 * 1024 * 1024))

