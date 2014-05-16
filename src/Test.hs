{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Main where

import Types.DslTypes (TriggerRaw(..), Any(..))
import Process.Configurator.Dsl  (parseTrigger)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Data.Text (Text, pack)
import Control.Applicative ((<$>),(<*>))
import Data.String (fromString)
import Data.Binary (Binary, decode, encode)
import Text.Peggy.Prim (ParseError)
import Data.Monoid (mempty)


main :: IO ()
main = defaultMain tests

mainWithOpts :: Int -> IO ()
mainWithOpts x = defaultMainWithOpts tests $ mempty { ropt_test_options = Just (mempty { topt_maximum_generated_tests = Just x })}

tests :: [Test]
tests = [ testProperty "Binary" checkBinary
        , testProperty "Parser" checkParse
        ]

checkBinary :: TriggerRaw Bool -> Bool
checkBinary = \x -> en x == x 

checkParse :: TriggerRaw Bool -> Bool
checkParse = \x -> parseTrigger (ps x) == Right x

instance Eq ParseError where
    (==) x y = undefined

instance Arbitrary (TriggerRaw Int) where
    arbitrary = Int <$> (arbitrary :: Gen Int)

allowed :: [Char] 
allowed = [ 'A' .. 'Z'] ++ [ 'a' .. 'z' ] ++ ['0' .. '9'] 

instance Arbitrary (TriggerRaw Text) where
        arbitrary = Text . fromString <$> (vectorOf 8 $ elements allowed)

instance Arbitrary Any where
    arbitrary = oneof [ Any <$> (Bool <$> (arbitrary :: Gen Bool))
                      , Any <$> (arbitrary :: Gen (TriggerRaw Int))
                      , Any <$> (arbitrary :: Gen (TriggerRaw Text))
                      ]

instance Arbitrary (TriggerRaw Bool) where
    arbitrary = oneof [ mle
                      , Not <$> mle
                      , And <$> mle <*> mle
                      , Or  <$> mle <*> mle
                      ] 
                where bool' = Bool <$> (arbitrary :: Gen Bool)
                      any' = arbitrary :: Gen Any
                      mle = oneof [ More <$> (arbitrary :: Gen (TriggerRaw Text)) <*> any' 
                                  , Less <$> (arbitrary :: Gen (TriggerRaw Text)) <*> any' 
                                  , Equal <$> (arbitrary :: Gen (TriggerRaw Text)) <*> any' 
                                  , bool'
                                  ]

en :: Binary a => a -> a
en = decode . encode

ps :: Show a => a -> Text
ps = pack . show

