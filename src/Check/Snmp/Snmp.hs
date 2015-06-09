{-# LANGUAGE OverloadedStrings #-}
module Check.Snmp.Snmp where

import qualified Data.Yaml as Y
import qualified Network.Protocol.Snmp as S
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Scientific (coefficient)
import Types.Dynamic
import Data.Text.Encoding
import Data.Monoid ((<>))
import           Data.Text             (chunksOf, pack)
import qualified Data.ByteString as BS
import           Numeric               (showHex)
import qualified Data.Text.Read as T
import qualified Data.Text as T
import Data.Either (rights)
-- import Debug.Trace


data SnmpDefinition = Bulk
  { oid   :: S.OID
  , bucket :: Text
  , tag   :: Text
  , bId   :: Text
  , names :: Map Integer ConvertRule
  } deriving Show

data ConvertRule = Replace
  { simple :: T.Text
  , convertFun :: S.Value -> S.Value
  , replaceAlias :: S.Value -> S.Value
  , replaceTag   :: S.Value -> S.Value
  }
instance Show ConvertRule where
  show x = "ConvertRule: simple = " 
        ++ show (simple x) 
 



newtype Rules = Rules (Map Text SnmpDefinition) deriving (Show)

check :: IO ()
check = do
    Just o <- Y.decodeFile "snmp.yaml" :: IO (Maybe Y.Value)
    print (convertRuleFromYuml o)

parseRules :: FilePath -> IO (Either String Rules)
parseRules f = do
    er <- Y.decodeFile f 
    case er of
         Nothing -> return $ Left "cant parse snmp.yaml"
         Just x -> return $ convertRuleFromYuml x

convertRuleFromYuml :: Y.Value -> Either String Rules
convertRuleFromYuml (Y.Object v) = do
    Y.Object snmp <- maybe (Left "empty snmp rule") Right $ HM.lookup "snmp" v
    let allNames = HM.keys snmp
    allRules <- mapM (getRule snmp) allNames
    return $ Rules $ M.fromList allRules
    where
      getRule :: HM.HashMap Text Y.Value -> Text -> Either String (Text, SnmpDefinition)
      getRule snmp name = do
          Y.Object rule <- maybe (Left "rule not found") Right $ HM.lookup name snmp
          Y.String request <-  maybe (Right "bulk") Right $ HM.lookup "request" rule
          Y.String oid' <- maybe (Left "oid not found") Right $ HM.lookup "oid" rule
          Y.String tag' <- maybe (Left "tag not found") Right $ HM.lookup "tag" rule
          Y.String subname <- maybe (Left "id not found") Right $ HM.lookup "id" rule
          namesArray <- maybe (Right Y.Null) Right $ HM.lookup "names" rule
          names' <-  getConvertRules namesArray
          case request of
               "bulk" -> return $ (name <> "." <> tag', Bulk (toOid oid') name tag' subname names')
               _ -> fail "bad request type"
      getConvertRules :: Y.Value -> Either String (Map Integer ConvertRule)
      getConvertRules Y.Null = Right M.empty
      getConvertRules (Y.Array xs) = return . M.fromList =<< (mapM getConvertRule $ V.toList xs)
      getConvertRules _ = Left "some thing wrong in getConvertRule"
      getConvertRule :: Y.Value -> Either String (Integer, ConvertRule)
      getConvertRule (Y.Object r) = do
          let key = filter (\(x, _) -> (x /= "alias" && x /= "tag_alias" && x /= "convert")) (HM.toList r)
          convert <- replaceFun $ HM.lookup "convert" r
          tagAlias <- replaceAlias' $ HM.lookup "tag_alias" r
          alias <- replaceAlias' $ HM.lookup "alias" r
          case key of
               [(k, Y.Number i)] -> Right (coefficient i, Replace k convert alias tagAlias)
               _ -> Left (show key ++ show (HM.toList r))
      getConvertRule _ = fail "not object in rule"
      replaceFun Nothing = return id
      replaceFun (Just "AsMac") = return $ convertByRule AsMac
      replaceFun x = fail $ "bad convert value: " ++ show x
      replaceAlias' :: Maybe Y.Value -> Either String (S.Value ->  S.Value)
      replaceAlias' Nothing = return id
      replaceAlias' (Just (Y.Object xs)) = return $ \x -> case (lookup x (map convertAlias $ HM.toList xs)) of
                                                                 Nothing -> x
                                                                 Just y -> y
      replaceAlias' x = fail $ "bad alias: " ++ show x
      convertAlias :: (Text, Y.Value) -> (S.Value, S.Value)
      convertAlias (t, Y.String s) = (S.String (encodeUtf8 s), S.String (encodeUtf8 t))
      convertAlias (t, Y.Number n) = (S.Integer (fromIntegral $ coefficient n), S.String (encodeUtf8 t))
      convertAlias _ = error "bad value in convertAlias"
      toOid :: Text -> S.OID
      toOid = map fst . rights . map T.decimal . T.splitOn (pack ".")
convertRuleFromYuml _ = Left "not object"

convertByRule :: Rule -> S.Value -> S.Value
convertByRule AsMac (S.String "") = S.String ""
convertByRule AsMac (S.String x) = S.String . encodeUtf8 . foldr1 (\a b -> a <> ":" <> b) . chunksOf 2 . pack . BS.foldr showHex "" $ x

convertByRule _ _ = error "convertByRule"


