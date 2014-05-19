{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Check where
import           Control.Applicative (Applicative)
import           Control.Monad
import           Control.Monad.State
import           Data.Map            (Map, insert, lookup)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Yaml.Builder
import           Prelude             hiding (lookup, putStr)

import           Types               (Check (..), Complex (..))

data AC  where
  AC :: Checkable a =>  (Check -> IO Complex, a) -> AC

type Route = Map Text AC

runCheckT :: Monad m => CheckT m a -> Route -> m a
runCheckT = evalStateT . runS

newtype CheckT m a = CheckT { runS :: StateT Route m a } deriving (Functor, Applicative, Monad, MonadIO, MonadState Route, MonadTrans)

addRoute :: (Monad m, Checkable a) =>  a -> CheckT m ()
addRoute x = modify fun
  where
    fun :: Route -> Route
    fun = insert (fst . route $ x) (AC (snd . route $ x, x))

runCheck :: (MonadIO m, Monad m) => Check -> CheckT m (Maybe Complex)
runCheck ch@(Check _ _ t _) = do
    routes <-  get 
    maybe (return Nothing)
          (\(AC (f,_)) -> do
              r <- liftIO $ f ch
              return $ Just r)
          (lookup t routes)

describeCheck :: Monad m => Check -> CheckT m YamlBuilder
describeCheck (Check _ _ t _) = do
    routes <- get 
    case lookup t routes of
         Nothing -> return $ string ""
         Just (AC (_, x)) -> return $ example [x]

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

type Description = Text
type Name = Text
type Field = Text
type Required = Bool

class Checkable a where
    route :: a -> (Name, Check -> IO Complex)
    describe :: a -> [(Field, Required, Description)]
    isCorrect :: Check -> a -> Either Text Check

    example :: Checkable a =>  [a] -> YamlBuilder
    example xs = mapping [("checks", array $ map example' xs)]

example' :: Checkable a => a -> YamlBuilder
example' a = let m = describe a
                 def = [("name", string "must be"), ("period", string "must be, in cron format")]
                 ty = [("type", string $ fst $ route a)]
                 nds = def <> ty <> map fun m
                 fun (t, b, d) = if b
                                    then (t, string $ "must be: " <> d)
                                    else (t, string $ "can be: " <> d)
             in mapping nds
{--
testHttp = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", "http://ya.ru")])

testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", ""), ("command", "uptime")])
--}
