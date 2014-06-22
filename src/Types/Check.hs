{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Check where
import           Control.Applicative (Applicative)
import           Control.Monad
import           Control.Monad.State
import           Data.Map            (Map, insert, lookup, keys)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Yaml.Builder
import           Prelude             hiding (lookup, putStr)

import           Types.Shared             (Check (..), Complex (..))

type Route = Map Text (Check -> IO Complex)

{--
describeCheck :: Monad m => Check -> CheckT m YamlBuilder
describeCheck (Check _ _ t _) = do
    routes <- get
    case lookup t routes of
         Nothing -> return $ string ""
         Just (AC (_, x)) -> return $ example [x]
    --}

type Description = Text
type Name = Text
type Field = Text
type Required = Bool

class Checkable a where
    route :: a -> Route
    describe :: a -> [(Field, Required, Description)]
    isCorrect :: Check -> a -> Either Text Check

    example :: Checkable a =>  [a] -> YamlBuilder
    example xs = mapping [("checks", array $ map example' xs)]

example' :: Checkable a => a -> YamlBuilder
example' a = let m = describe a
                 def = [("name", string "must be"), ("period", string "must be, in cron format")]
                 ty = [("type", string $ head $ keys $ route a)]
                 nds = def <> ty <> map fun m
                 fun (t, b, d) = if b
                                    then (t, string $ "must be: " <> d)
                                    else (t, string $ "can be: " <> d)
             in mapping nds
{--
testHttp = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", "http://ya.ru")])

testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", ""), ("command", "uptime")])
--}
