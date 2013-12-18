{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE GADTs #-}
module GNS.Trigger
where

import           Control.Applicative  hiding (empty)
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as A
import qualified Data.ListLike        as LL
import           Data.Map             (lookup)
import qualified Data.Map             as Map
import           Data.Text            hiding (empty, filter, foldl1, head, map)
import           Data.Text.Encoding   (encodeUtf8)
import           Debug.Trace          (trace)
import           GNS.Data
import           Prelude              hiding (and, lookup, not, or)
import qualified Prelude              as P
import           Text.Peggy           hiding (And, Not)
import qualified Text.Peggy           as Peg

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

data Any where 
  Any :: (Eq a, Ord a, Show a) => FFF a -> Any

instance Show Any where
    show (Any x) = show x

data FFF a where
  Int :: Int -> FFF Int
  Bool :: Bool -> FFF Bool
  Text :: Text -> FFF Text

  NotF :: FFF Bool -> FFF Bool
  OrF :: FFF Bool -> FFF Bool -> FFF Bool
  AndF :: FFF Bool -> FFF Bool -> FFF Bool

  LessF :: FFF Text -> Any -> FFF Bool
  MoreF :: FFF Text -> Any -> FFF Bool
  EqualF :: FFF Text -> Any -> FFF Bool

instance (Show a) => Show (FFF a) where
    show (Text x) = show x
    show (EqualF x y) = show x ++ " equal " ++ show y

a = Text "key"
b = Any $ Bool True
c = EqualF a b

eval :: FFF a ->  Complex -> Status
eval (EqualF x y) (Complex c) =  undefined 

[peggy|

top :: FFF Bool = expr

expr :: FFF Bool
  = pName "equal" pReturn { EqualF $1 $2 }

pName :: FFF Text
  =  pChar* { Text (pack $1) }

pChar :: Char = [0-9a-zA-Z]

pReturn :: Any
  = "true" { Any $ Bool True }
  / "True" { Any $ Bool True }
  / "false" { Any $ Bool False }
  / "False" { Any $ Bool False }
  / num     { Any $ Int $1 }
  / pName   { Any $ $1 }

num ::: Int
  = [0-9]* { read $1 }

|]

{-
evalF :: FFF a -> a
evalF (Num a) = a
evalF (Bool a) = a
evalF (Str a) = a
evalF (Plus a1 a2) = evalF a1 + evalF a2
evalF (NotF a) = P.not $ evalF a
evalF (OrF a1 a2) = evalF a1 || evalF a2
evalF (AndF a1 a2) = evalF a1 && evalF a2
-}

instance Show GnsFun where
    show (Less n r) = show n ++ " less " ++ show r
    show (More n r) = show n ++ " more " ++ show r
    show (Equal n r) = show n ++ " equal " ++ show r
    show (Not x ) = "not " ++ show x
    show (Or x) = Prelude.foldl1 (\a b -> a ++ " or " ++ b) $ map show x
    show (And x) = Prelude.foldl1 (\a b -> a ++ " and " ++ b) $ map show x

{--
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
  = [0-9]* { A.I (read $1) }

|]

evalGns :: GnsFun -> Complex -> Status
evalGns (Less n r) (Complex c) = Status $ maybe False (< r) (lookup n c)
evalGns (More n r) (Complex c) = Status $ maybe False (> r) (lookup n c)
evalGns (Equal n r) (Complex c) = Status $ maybe False (== r) (lookup n c)
evalGns (Not g) c = not $ evalGns g c
evalGns (Or g) c = or $ map (\x -> evalGns x c) g
evalGns (And g) c = and $ map (\x -> evalGns x c) g

parseTrigger :: Text -> Either ParseError TriggerFun
parseTrigger x = (parseString top "<stdin>" $ LL.CS $ encodeUtf8 x) >>= P.return . TriggerFun . evalGns
-- parseFun x = do
--     y <- (parseString top "<stdin>" $ LL.CS $ encodeUtf8 x)
--     trace (show y) $ P.return . TriggerFun . evalGns $ y

--}
