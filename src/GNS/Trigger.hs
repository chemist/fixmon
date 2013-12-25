{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE GADTs #-}
module GNS.Trigger
where

import qualified Data.ListLike        as LL
import           Data.Map             (lookup)
import           Data.Text            hiding (empty, filter, foldl1, head, map)
import           Data.Text.Encoding   (encodeUtf8)
import           GNS.Data
import           Prelude              hiding (and, lookup, not, or)
import qualified Prelude              as P
import           Text.Peggy           hiding (And, Not)



[peggy|

top :: FFF Bool = expr

expr :: FFF Bool
  = expr "and" simpl { And $1 $2 }
  / expr "or"  simpl { Or  $1 $2 }
  / "not" expr { Not $1 }
  / simpl { $1 }

simpl :: FFF Bool 
  = pName "equal" pReturn { Equal $1 $2 }
  / pName "more"  pReturn { More  $1 $2 }
  / pName "less"  pReturn { Less  $1 $2 }

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


eval :: FFF a -> Complex -> Status
eval (Less (Text x) y) (Complex c) = Status $ maybe False (< y) (lookup x c)
eval (More (Text x) y) (Complex c) = Status $ maybe False (> y) (lookup x c)
eval (Equal (Text x) y) (Complex c) = Status $ maybe False (== y) (lookup x c)
eval (Not x) c = not $ eval x c
eval (And x y) c = eval x c `and` eval y c
eval (Or x y) c = eval x c `or` eval y c
eval _ _ = Status False


not :: Status -> Status
not (Status x) = Status $ P.not x

or :: Status -> Status -> Status
or (Status a) (Status b) = Status $ a || b

and :: Status -> Status -> Status
and (Status a) (Status b) = Status $ a && b



parseTrigger :: Text -> Either ParseError TriggerFun
parseTrigger x = (parseString top "<stdin>" $ LL.CS $ encodeUtf8 x) >>= P.return . TriggerFun . eval


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
