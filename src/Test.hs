{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Main where

import Types.Dynamic (Exp(..), Dyn(..))
import Process.Configurator.Dsl  (parseTrigger)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Data.Text (Text, pack)
import Control.Applicative ((<$>),(<*>))
import Data.String (fromString)
import Data.Binary (Binary, decode, encode)
import Data.Monoid (mempty)


main :: IO ()
main = defaultMain tests

mainWithOpts :: Int -> IO ()
mainWithOpts x = defaultMainWithOpts tests $ mempty { ropt_test_options = Just (mempty { topt_maximum_generated_tests = Just x })}

tests :: [Test]
tests = []

