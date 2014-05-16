{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Check where
import Data.Maybe (fromJust)
import Data.Map (insert, Map, lookup, empty, fromList)
import Data.Text (Text, unpack)
import Control.Applicative ((<$>), (<*>), (*>), Applicative)
import Data.Monoid ((<>))
import System.Cron
import Types (Check(..), Complex(..))
import Control.Monad.State
import Control.Monad
import Data.Vector (Vector)
import Network.URI
import Data.Yaml.Builder
import Data.ByteString (putStr)
import Prelude hiding (lookup, putStr)

data AC  where
  AC :: Checkable a =>  ((Check -> IO Complex), a) -> AC

type Route = Map Text AC

runCheckT :: CheckT IO a -> Route -> IO a
runCheckT = evalStateT . runS

newtype CheckT m a = CheckT { runS :: StateT Route m a } deriving (Functor, Applicative, Monad, MonadIO, MonadState Route)

addRoute :: Checkable a =>  a -> CheckT IO ()
addRoute x = modify fun
  where
    fun :: Route -> Route
    fun = insert (fst . route $ x) (AC (snd . route $ x, x))

runCheck :: Check -> CheckT IO (Maybe Complex)
runCheck ch@(Check _ _ t _) = do
    routes <-  get :: CheckT IO Route 
    maybe (return Nothing) 
          (\(AC (f,_)) -> do
              r <- liftIO $ f ch 
              return $ Just r) 
          (lookup t routes)

describeCheck :: Check -> CheckT IO YamlBuilder
describeCheck ch@(Check _ _ t _) = do
    routes <- get :: CheckT IO Route
    case lookup t routes of
         Nothing -> return $ string ""
         Just (AC (_, t)) -> return $ example t

{--
abr :: CheckT IO ()
abr = do
    addRoute Http
    addRoute Shell
    runCheck testHttp
    x <- describeCheck testHttp
    liftIO $ putStr $ toByteString x
    return ()
    --}

class Checkable a where
    route :: a -> (Text, Check -> IO Complex) 
    describe :: a -> [(Text, Bool, Text -> Maybe Text)]

example :: Checkable a => a -> YamlBuilder
example a = let m = describe a
                def = [("name", string "must be"), ("period", string "must be, in cron format")]
                ty = [("type", string $ fst $ route a)]
                nds = def <> ty <> map fun m
                fun (t, b, _) = if b 
                                   then (t, string "must be")
                                   else (t, string "can be")

            in mapping [("checks", mapping nds)]

{--
data Http = Http deriving Show
data Shell = Shell deriving Show

instance Checkable Http where
    route Http = ("http.status", doHttp)
    describe Http = [("url", True, checkUrl)]

instance Checkable Shell where
    route Shell = ("cmd.run", doShell)
    describe Shell = [("command", True, checkCommand)]

doHttp :: Check -> IO Complex
doHttp c = print "do http check" >> (return $ Complex empty)

checkUrl :: Text -> Maybe Text
checkUrl x = if isURI (unpack x)
                then Nothing
                else (Just "cant parse url")

checkCommand _ = Nothing

doShell :: Check -> IO Complex
doShell c = print "do shell check" >> (return $ Complex empty)

testHttp = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", "http://ya.ru")])

testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", ""), ("command", "uptime")])
--}
