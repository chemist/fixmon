{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
module Types.Check where
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict     (Map, keys, singleton, empty)
import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Yaml           hiding (array)
import           Data.Yaml.Builder
import           Prelude             hiding (lookup, putStr)

import           Types.Dynamic       (Complex, Counter, Dyn)
import           Types.Shared        (Check (..))
import           Check.Snmp.Snmp (Rules(..))

type Route = Map Text (Check -> IO Complex)

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
    route :: Rules -> a -> Route
    describe :: a -> [(Counter, Required, CheckValue, Description)]
    routeCheck :: Rules -> a -> RouteCheck

    example :: [a] -> YamlBuilder
    example xs = mapping [("checks", array $ map example' xs)]

type Problem = String

routeCheck' :: Checkable a => a -> Text -> RouteCheck
routeCheck' x checkT = singleton checkT $! fun (describe x)
  where
    fun :: [(Counter, Required, CheckValue, Description)] -> Check -> Either String Check
    fun desc check =
      let Object params = cparams check
          checking = map tryCheck desc
          eitherToMaybeProblem (Left y) = Just y
          eitherToMaybeProblem (Right _) = Nothing
          tryCheck :: (Counter, Required, CheckValue, Description) -> Maybe Problem
          tryCheck (field, isMustBe, cv, _) =
              case (HM.member field params, isMustBe)  of
                   (True, True) -> eitherToMaybeProblem $ cv (params HM.! field )
                   (True, False) -> eitherToMaybeProblem $ cv (params HM.! field)
                   (False, True) -> Just $ "field not found " <> show field
                   (False, False) -> Nothing
          result :: [Maybe Problem] -> Either String Check
          result r = case catMaybes r of
                          [] -> Right check
                          xs -> Left $ foldr1 (\z y -> z ++ " " ++ y) xs
      in result checking

example' :: Checkable a => a -> YamlBuilder
example' a = let m = describe a
                 def = [("name", string "must be"), ("period", string "must be, in cron format")]
                 ty = [("type", string $ head $ keys $ route (Rules empty) a)]
                 nds = def <> ty <> map fun m
                 fun (t, b, _, d) = if b
                                    then (t, string $ "must be: " <> d)
                                    else (t, string $ "can be: " <> d)
             in mapping nds

