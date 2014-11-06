{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types.Check where
import           Data.Map.Strict          (Map, keys, singleton)
import           Data.Maybe
import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import           Data.Yaml.Builder
import           Prelude           hiding (lookup, putStr)
import Data.Lists
import Network.Protocol.Snmp (Suite)

import           Types.Shared      (Check (..))
import           Types.Dynamic     (Complex(..), Dyn(..), Counter(..))

type Route = Map Text (Check -> IO [Complex])

type RouteCheck = Map Text (Check -> Either String Check)

{--
describeCheck :: Monad m => Check -> CheckT m YamlBuilder
describeCheck (Check _ _ t _) = do
    routes <- get
    case lookup t routes of
         Nothing -> return $ string ""
         Just (AC (_, x)) -> return $ example [x]
    --}
    --

type Description = Text
type Name = Text
type Required = Bool
type CheckValue = Dyn -> Either String Dyn 

class Checkable a where
    route :: a -> Route
    describe :: a -> [(Counter, Required, CheckValue, Description)]
    routeCheck :: a -> RouteCheck

    example :: [a] -> YamlBuilder
    example xs = mapping [("checks", array $ map example' xs)]

class ToComplex a where
    complex :: a -> Complex
    convert :: Suite -> [a]

type Problem = String

routeCheck' :: Checkable a => a -> Text -> RouteCheck
routeCheck' x checkT = singleton checkT $! fun (describe x)
  where
    fun :: [(Counter, Required, CheckValue, Description)] -> Check -> Either String Check
    fun desc check =
      let params = cparams check
          checking = map tryCheck desc
          eitherToMaybeProblem (Left y) = Just y
          eitherToMaybeProblem (Right _) = Nothing
          tryCheck :: (Counter, Required, CheckValue, Description) -> Maybe Problem
          tryCheck (field, isMustBe, cv, _) =
              case (hasKeyAL field params, isMustBe)  of
                   (True, True) -> eitherToMaybeProblem $ cv (fromJust $ lookup field params)
                   (True, False) -> eitherToMaybeProblem $ cv (fromJust $ lookup field params)
                   (False, True) -> Just $ "field not found " <> show field
                   (False, False) -> Nothing
          result :: [Maybe Problem] -> Either String Check
          result r = case catMaybes r of
                          [] -> Right check
                          xs -> Left $ foldr1 (\z y -> z ++ " " ++ y) xs
      in result checking

example' :: Checkable a => a -> YamlBuilder
example' a = let m = describe a
                 def = [(Counter "name", string "must be"), (Counter "period", string "must be, in cron format")]
                 ty = [(Counter "type", string $ head $ keys $ route a)]
                 nds = def <> ty <> map fun m
                 fun (t, b, _, d) = if b
                                    then (t, string $ "must be: " <> d)
                                    else (t, string $ "can be: " <> d)
             in mapping $ map (\(x, y) -> (unCounter x, y)) nds

unCounter :: Counter -> Text
unCounter (Counter x) = x

