{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types.Check where
import           Data.Map            (Map, keys, singleton, (!))
import qualified Data.Map as M
import Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unpack)
import           Data.Yaml.Builder
import Data.Dynamic
import           Prelude             hiding (lookup, putStr)

import           Types.Shared             (Check (..), Complex (..))

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

type Description = Text
type Name = Text
type Field = Text
type Required = Bool
type CheckValue = Dynamic -> Either String Dynamic

class Checkable a where
    route :: a -> Route
    describe :: a -> [(Field, Required, CheckValue, Description)]
    routeCheck :: a -> RouteCheck

    example :: [a] -> YamlBuilder
    example xs = mapping [("checks", array $ map example' xs)]

type Problem = String

routeCheck' :: Checkable a => a -> Text -> RouteCheck
routeCheck' x checkT = singleton checkT $ fun (describe x)
  where 
    fun :: [(Field, Required, CheckValue, Description)] -> Check -> Either String Check
    fun desc check = 
      let params = cparams check
          checking = map tryCheck desc
          eitherToMaybeProblem (Left y) = Just y
          eitherToMaybeProblem (Right _) = Nothing
          tryCheck :: (Field, Required, CheckValue, Description) -> Maybe Problem
          tryCheck (field, isMustBe, cv, _) = 
              case (M.member field params, isMustBe)  of
                   (True, True) -> eitherToMaybeProblem $ cv (params ! field)
                   (True, False) -> eitherToMaybeProblem $ cv (params ! field)
                   (False, True) -> Just $ "field not found " <> unpack field
                   (False, False) -> Nothing
          result :: [Maybe Problem] -> Either String Check
          result r = case catMaybes r of
                          [] -> Right check
                          xs -> Left $ foldl1 (\z y -> z ++ " " ++ y) xs
      in result checking
         
--           checkMustKeys = map M.member  filter (\(_, x, _, _) -> x) desc


example' :: Checkable a => a -> YamlBuilder
example' a = let m = describe a
                 def = [("name", string "must be"), ("period", string "must be, in cron format")]
                 ty = [("type", string $ head $ keys $ route a)]
                 nds = def <> ty <> map fun m
                 fun (t, b, _, d) = if b
                                    then (t, string $ "must be: " <> d)
                                    else (t, string $ "can be: " <> d)
             in mapping nds

{--
isCorrect :: Check -> Either String Check
isCorrect check = 
    let checkT = checkType check 
        isCorrect' [] = Right check
    in isCorrect' (describe checkT) 
    --}
{--
testHttp = Check (CheckName "web") (Cron daily) "http.status" (fromList [("url", "http://ya.ru")])

testShell = Check (CheckName "shell") (Cron daily) "cmd.run" (fromList [("abc", ""), ("command", "uptime")])
--}
