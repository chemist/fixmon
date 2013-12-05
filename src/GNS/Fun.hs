{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GNS.Fun where

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
import Text.Peggy hiding  (Not, And)
import qualified Text.Peggy as Peg

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

instance Show GnsFun where
    show (Less n r) = show n ++ " less " ++ show r
    show (More n r) = show n ++ " more " ++ show r
    show (Equal n r) = show n ++ " equal " ++ show r
    show (Not x ) = "not " ++ show x
    show (Or x) = Prelude.foldl1 (\a b -> a ++ " or " ++ b) $ map show x
    show (And x) = Prelude.foldl1 (\a b -> a ++ " and " ++ b) $ map show x

[peggy|
top :: GnsFun = expr 

expr :: GnsFun
  = expr "or" fact { Or [$1, $2] }
  / expr "and" fact { And [$1, $2] }
  / "not" expr { Not $1 }
  / fact { $1 }

fact :: GnsFun
  = pName "equal" pReturn { Equal $1 $2 }
  / pName "more" pReturn { More $1 $2 }
  / pName "less" pReturn { Less $1 $2 }

pName :: Name
  = pChar* { Name (pack $1) }

pChar :: Char = [0-9a-zA-Z]

pReturn :: Return
  = "true" { CB True }
  / "True" { CB True }
  / "false" { CB False }
  / "False" { CB False }
  / num     { CI $1 }
  
num ::: A.Number
  = [1-9]* { A.I (read $1) }

|]

evalGns :: GnsFun -> Complex -> Status
evalGns (Less n r) (Complex c) = Status $ maybe False (< r) (lookup n c)
evalGns (More n r) (Complex c) = Status $ maybe False (> r) (lookup n c)
evalGns (Equal n r) (Complex c) = Status $ maybe False (== r) (lookup n c)
evalGns (Not g) c = not $ evalGns g c
evalGns (Or g) c = or $ map (\x -> evalGns x c) g
evalGns (And g) c = and $ map (\x -> evalGns x c) g

tf :: GnsFun -> TriggerFun
tf x = TriggerFun $ evalGns x

a :: String
a = show $ And [ Less (Name $ pack "name") (CB True), Equal (Name $ pack "eqname") (CB True)]

main :: IO ()
main = print . parseString top "<stdin>" =<< getContents
