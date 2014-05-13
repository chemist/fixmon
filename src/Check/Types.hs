{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Check.Types where
import Data.Maybe (fromJust)
import Data.Map (insert, Map, lookup, empty, fromList)
import Data.Text (Text)
import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>), (*>), Applicative)
import Data.Monoid ((<>))
import System.Cron
import Types
import Control.Monad.State
import Control.Monad
import Data.Vector (Vector)

data CC where
  CC :: Checkable a =>  ((Check -> IO Complex), a) -> CC

type Route = Map Text CC

runCheckT :: CheckT IO a -> Route -> IO a
runCheckT = evalStateT . runS

newtype CheckT m a = CheckT { runS :: StateT Route m a } deriving (Functor, Applicative, Monad, MonadIO, MonadState Route)

addRoute :: Checkable a =>  a -> CheckT IO ()
-- addRoute x = modify $ insert (fst . route $ x) (snd . route $ x)
addRoute x = modify fun
  where
    fun :: Route -> Route
    fun = insert (fst . route $ x) (CC (snd . route $ x, x))

runCheck :: Check -> CheckT IO (Maybe Complex)
runCheck ch@(Check _ _ t _) = do
    routes <-  get :: CheckT IO Route 
    maybe (return Nothing) 
          (\(CC (f,_)) -> do
              r <- liftIO $ f ch 
              return $ Just r) 
          (lookup t routes)

checkType :: Check -> CheckT IO Text
checkType ch@(Check _ _ t _) = do
    routes <- get :: CheckT IO Route
    case lookup t routes of
         Nothing -> return $ ""
         Just (CC (_, t)) -> return $ example t

abr :: CheckT IO ()
abr = do
    addRoute Http
    addRoute Shell
    runCheck testHttp
    x <- checkType testShell
    liftIO $ print x
    return ()

class Checkable a where
    route :: a -> (Text, Check -> IO Complex) 
    checkMap :: a -> [(Text, Bool)]

example :: Checkable a => a -> Text
example a = let m = checkMap a
                must = foldl (\s (t, b) -> s <> if b then t else "") "must be: " m
                can  = foldl (\s (t, b) -> s <> if b then "" else t) " can be: " m
            in must <> can

data Http = Http deriving Show
data Shell = Shell deriving Show

instance Checkable Http where
    route Http = ("http.status", doHttp)
    checkMap Http = [("url", True)]

instance Checkable Shell where
    route Shell = ("cmd.run", doShell)
    checkMap Shell = [("command", True)]

doHttp :: Check -> IO Complex
doHttp c = print "do http check" >> (return $ Complex empty)

doShell :: Check -> IO Complex
doShell c = print "do shell check" >> (return $ Complex empty)

testHttp = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", "http://ya.ru")])

testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", ""), ("command", "uptime")])
