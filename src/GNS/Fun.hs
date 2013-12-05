{-# LANGUAGE OverloadedStrings #-}
module GNS.Fun (parseFun) where

import           Control.Applicative hiding (empty)
import           Data.Attoparsec.Text (parseOnly, Parser)
import qualified Data.Attoparsec.Text as A
import           Data.Text            hiding (filter, head, map, empty, foldl1)
import           GNS.Data
import qualified Data.Map as Map 
import Data.Map (lookup)
import Prelude hiding (lookup, not, and, or)
import qualified Prelude as P
import Debug.Trace (trace)

parseFun :: Text -> TriggerFun
parseFun x = trace (unpack x) $ si


si :: TriggerFun 
si = TriggerFun (\_ -> Status True)

{--
names: less more equal 
bool logic: or and not

str : Name Fun Return or Name Fun Return

--}

not :: Status -> Status
not (Status x) = Status $ P.not x

or :: [Status] -> Status
or = foldl1 $ \(Status a) (Status b) -> Status $ a || b 

and :: [Status] -> Status
and = foldl1 $ \(Status a) (Status b) -> Status $ a && b

data GnsFun = Less { name :: Name, return :: Return }
            | More { name :: Name, return :: Return }
            | Equal { name :: Name, return :: Return }
            | Not GnsFun
            | Or [GnsFun]
            | And [GnsFun]

evalGns :: GnsFun -> Complex -> Status
evalGns (Less n r) (Complex c) = Status $ maybe False (< r) (lookup n c)
evalGns (More n r) (Complex c) = Status $ maybe False (> r) (lookup n c)
evalGns (Equal n r) (Complex c) = Status $ maybe False (== r) (lookup n c)
evalGns (Not g) c = not $ evalGns g c
evalGns (Or g) c = or $ map (\x -> evalGns x c) g
evalGns (And g) c = and $ map (\x -> evalGns x c) g

tf :: GnsFun -> TriggerFun
tf x = TriggerFun $ evalGns x


bool :: Parser Return
bool =  sp (A.string "True" <|> A.string "true" <|> A.string "ok") *> pure (CB True)
     <|> sp (A.string "False"  <|> A.string "false") *> pure (CB False)

num :: Parser Return
num = CI <$> (sp A.number)

sp :: Parser a -> Parser a
sp a = A.skipSpace *> a <* A.skipSpace

less :: Parser GnsFun
less = do
    sp (A.string "less") *> pure (<)
    undefined

pname :: Parser Name
pname = A.takeWhile1 (not isSpace) 

{--
or :: Parser ([Bool] -> Bool)
or = sp (A.string "or")  *> pure Prelude.or 

not :: Parser (Bool -> Bool)
not = sp (A.string "not") *> pure Prelude.not

and :: Parser ([Bool] -> Bool)
and = sp (A.string "and") *> pure Prelude.and


more :: Parser (Return -> Return -> Bool)
more = sp (A.string "more") *> pure (>)

equal :: Parser (Return -> Return -> Bool)
equal = sp (A.string "equal") *> pure (==)

end :: Parser () 
end = A.endOfInput

cm :: Complex 
cm = Complex $ Map.fromList [("return", CI 1), ("status", CB True)]


parseReturn :: Text -> Return
parseReturn x = let (Right a) = trace (unpack x) $ parseOnly parse' x 
                in a

simple :: Parser Return
simple = let simple' = bool <|> num -- <|> parserText 
         in simple' <|> (Not <$> (not *> simple'))

parseOr = Or <$> (A.sepBy1 simple or) <* end 

parse' = (simple <* end ) <|> parseOr

parserText = undefined

--}
