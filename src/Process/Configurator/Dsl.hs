{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GADTs #-}
module Process.Configurator.Dsl
where

import           Types

import qualified Data.ListLike        as LL
import           Data.Map             (lookup)
import           Data.Text            hiding (empty, filter, foldl1, head, map)
import           Data.Text.Encoding   (encodeUtf8)
import           Prelude              hiding (and, lookup, not, or)
import qualified Prelude              as P
import           Text.Peggy           hiding (And, Not)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Error as E
import Text.Read (readMaybe)
import Control.Applicative ((<$>), (<*>), pure)



[peggy|

top :: TriggerRaw Bool = expr

expr :: TriggerRaw Bool
  = expr "and" simpl { And $1 $2 }
  / expr "or"  simpl { Or  $1 $2 }
  / "not" expr { Not $1 }
  / simpl { $1 }

simpl :: TriggerRaw Bool 
  = pName "equal" pReturn { Equal $1 $2 }
  / pName "more"  pReturn { More  $1 $2 }
  / pName "less"  pReturn { Less  $1 $2 }
  / pBool

pName :: TriggerRaw Text
  =  [a-zA-Z0-9]+ { Text (pack $1) }

pBool :: TriggerRaw Bool
  = "false" { Bool False }
  / "False" { Bool False }
  / "true"  { Bool True }
  / "True"  { Bool True }

pReturn :: Any
  = "true" { Any $ Bool True }
  / "True" { Any $ Bool True }
  / "false" { Any $ Bool False }
  / "False" { Any $ Bool False }
  / num     { Any $ Int $1 }
  / pName   { Any $ $1 }

num ::: Int
  = [0-9]+ { read $1 }
  / [-] [0-9]+ { read ($1:$2) }

|]

parseTrigger :: Text -> Either ParseError (TriggerRaw Bool)
parseTrigger x = (parseString top "<stdin>" $ LL.CS $ encodeUtf8 x) >>= P.return 


data Env = Env 

type Eval a = R.ReaderT Env (E.ErrorT String IO) a


runEval :: Env -> Eval (TriggerRaw a) -> Complex -> IO (Either String Status)
runEval env e c = E.runErrorT (R.runReaderT (fun c e) env)

fun :: Complex -> Eval (TriggerRaw a) -> Eval Status
fun (Complex c) e = do
    t <- e
    case t of
         Less (Text x) y -> return $ Status $ maybe False (< y) (lookup x c)
         More (Text x) y -> return $ Status $ maybe False (> y) (lookup x c)
         Equal (Text x) y -> return $ Status $ maybe False (== y) (lookup x c)
         Not x -> not <$> (fun (Complex c) (pure x))
         And x y -> and <$> (fun (Complex c) (pure x)) <*> (fun (Complex c) (pure y))
         Or x y -> or <$> (fun (Complex c) (pure x)) <*> (fun (Complex c) (pure y))
         _ -> pure $ Status False

eval :: TriggerRaw a -> Complex -> Status
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
