{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Check.Snmp.Snmp where

import           Control.Monad         (mzero)
import qualified Data.ByteString       as BS
import           Data.Either           (rights)
import qualified Data.HashMap.Strict   as HM
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Monoid           ((<>))
import           Data.Scientific       (coefficient)
import           Data.Text             (Text)
import           Data.Text             (chunksOf, pack)
import qualified Data.Text             as T
import           Data.Text.Encoding
import qualified Data.Text.Read        as T
import qualified Data.Vector           as V
import           Data.Yaml             ((.!=), (.:), (.:?))
import qualified Data.Yaml             as Y
import qualified Network.Protocol.Snmp as S
import qualified Network.Protocol.Snmp as Snmp
import           Network.Snmp.Client   (Config)
import qualified Network.Snmp.Client   as Snmp
import           Numeric               (showHex)
import           Types.Dynamic


data SnmpDefinition = Bulk
  { oid    :: S.OID
  , config :: Config
  , bucket :: Text
  , tag    :: Text
  , names  :: Map Integer ConvertRule
  } deriving Show

data ConvertRule = Replace
  { simple       :: T.Text
  , convertFun   :: S.Value -> S.Value
  , replaceAlias :: S.Value -> S.Value
  , replaceTag   :: S.Value -> S.Value
  }
instance Show ConvertRule where
  show x = "ConvertRule: simple = "
        ++ show (simple x)

newtype Rules = Rules (Map Text SnmpDefinition) deriving (Show)

instance Y.FromJSON Rules where
    parseJSON (Y.Object v) = do
        Y.Object snmp <- v Y..: "snmp"
        let allNames = HM.keys snmp
        allRules <- mapM (getRule snmp) allNames
        return $ Rules $ M.fromList allRules
        where
          getRule :: HM.HashMap Text Y.Value -> Text -> Y.Parser (Text, SnmpDefinition)
          getRule snmp name = do
              Y.Object rule <- snmp Y..: name
              Y.String request <-  rule Y..: "request"
              Y.String oid' <- rule Y..: "oid"
              config' <- rule Y..: "config"
              Y.String tag' <- rule Y..: "tag"
              namesArray <- rule Y..:? "names" Y..!= Y.Null
              names' <-  getConvertRules namesArray
              case request of
                   "bulk" -> return $ (name <> "." <> tag', Bulk (toOid oid') config' name tag' names')
                   _ -> fail "bad request type"
          getConvertRules :: Y.Value -> Y.Parser (Map Integer ConvertRule)
          getConvertRules Y.Null = return M.empty
          getConvertRules (Y.Array xs) = return . M.fromList =<< (mapM getConvertRule $ V.toList xs)
          getConvertRules _ = fail "some thing wrong in getConvertRule"
          getConvertRule :: Y.Value -> Y.Parser (Integer, ConvertRule)
          getConvertRule (Y.Object r) = do
              let key = filter (\(x, _) -> (x /= "alias" && x /= "tag_alias" && x /= "convert")) (HM.toList r)
              convert <- replaceFun $ HM.lookup "convert" r
              tagAlias <- replaceAlias' $ HM.lookup "tag_alias" r
              alias <- replaceAlias' $ HM.lookup "alias" r
              case key of
                   [(k, Y.Number i)] -> return $ (coefficient i, Replace k convert alias tagAlias)
                   _ -> fail (show key ++ show (HM.toList r))
          getConvertRule _ = fail "not object in rule"
          replaceFun Nothing = return id
          replaceFun (Just "AsMac") = return $ convertByRule AsMac
          replaceFun x = fail $ "bad convert value: " ++ show x
          replaceAlias' :: Maybe Y.Value -> Y.Parser (S.Value ->  S.Value)
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
    parseJSON _ = fail "not object"

instance Y.FromJSON Snmp.Config where
    parseJSON (Y.Object v) = do
        ver <- v .:? "version" .!= (3 :: Int)
        case ver of
             3 -> parseVersion3
             2 -> parseVersion2
             _ -> error "bad snmp version"
        where
            parseVersion3 = do
                (sn :: Text) <- v .: "sequrityName"
                pt <- v .:? "privType" .!= "DES"
                pa <- v .:? "privAuth" .!= "AuthPriv"
                at <- v .:? "authType" .!= "MD5"
                p <-  v .:? "port"     .!= "161"
                t <-  v .:? "timeout"  .!= (5 :: Int)
                ap <- v .: "authPass"
                pp <- v .:? "privPass" .!= ap
                return $ Snmp.ConfigV3 ""
                                       p
                                       (t * 1000000)
                                       (encodeUtf8 sn)
                                       (encodeUtf8 ap)
                                       (encodeUtf8 pp)
                                       (encodePrivAuth pa)
                                       ""
                                       (encodeAuthType at)
                                       (encodePrivType pt)
            parseVersion2 = do
                (sn :: Text) <- v .: "community" .!= "public"
                p <-  v .:? "port"     .!= "161"
                t <-  v .:? "timeout"  .!= (5 :: Int)
                return $ Snmp.ConfigV2 "" p (t * 1000000) (Snmp.Community $ encodeUtf8 sn)

    parseJSON _ = mzero


encodePrivType :: Text -> Snmp.PrivType
encodePrivType "AES" = Snmp.AES
encodePrivType "DES" = Snmp.DES
encodePrivType _ = error "bad priv type"

encodeAuthType :: Text -> Snmp.AuthType
encodeAuthType "MD" = Snmp.MD5
encodeAuthType "MD5" = Snmp.MD5
encodeAuthType "SHA" = Snmp.SHA
encodeAuthType _ = error "bad priv type"

encodePrivAuth :: Text -> Snmp.PrivAuth
encodePrivAuth "NoAuthNoPriv" = Snmp.NoAuthNoPriv
encodePrivAuth "AuthNoPriv" = Snmp.AuthNoPriv
encodePrivAuth "AuthPriv" = Snmp.AuthPriv
encodePrivAuth _ = error "bad priv auth"


convertByRule :: Rule -> S.Value -> S.Value
convertByRule AsMac (S.String "") = S.String ""
convertByRule AsMac (S.String x) = S.String . encodeUtf8 . foldr1 (\a b -> a <> ":" <> b) . chunksOf 2 . pack . BS.foldr showHex "" $ x

convertByRule _ _ = error "convertByRule"


