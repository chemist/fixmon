{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage.InfluxDB where

import           Control.Exception         (catch, throw)
import           Control.Monad             (unless, when)
import           Data.Aeson                (FromJSON (..), ToJSON (..),
                                            Value (..), decode, encode, object,
                                            (.:), (.=))
import           Data.ByteString.Char8     (ByteString, pack)
import           Data.ByteString.Lazy      (fromChunks)
import           Data.Conduit              (($$+-))
import           Data.Conduit.List         (consume)
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.Scientific           (floatingOrInteger)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import qualified Data.Vector               as V
import qualified Data.Yaml                 as A
import           Network.HTTP.Conduit      hiding (host, port)
import           Network.HTTP.Types.Status
import           Prelude                   hiding (putStr)
import           Types.Dynamic
import           Types.Shared              (Check (ctype, cname),
                                            TaskResult (..))
-- import Debug.Trace


type DB = String
type User = String
type Password = String
type Host = String
type Port = Int
type EnableSSL = Bool

data InfluxDB = InfluxDB
  { base :: !DB
  , user :: !User
  , pass :: !Password
  , host :: !Host
  , port :: !Port
  , ssl  :: !EnableSSL
  } deriving Show

config :: InfluxDB
config = InfluxDB "fixmon" "fixmon" "fixmon" "fixmon" 8086 False

data InfluxQueryType = IWrite | IQuery

instance Show InfluxQueryType where
    show IWrite = "/write"
    show IQuery = "/query"

influxUrl :: InfluxQueryType -> InfluxDB -> String
influxUrl itype db =
   let scheme = if ssl db
                   then "https://"
                   else "http://"
       port' = show . port $ db
   in scheme <> host db <> ":" <> port' <> show itype

data Series = Series
    { seriesName :: !Text
    , seriesData :: !SeriesData
    } deriving (Show, Eq)

instance ToJSON Series where
   toJSON Series {..} = object
     [ "name" .= seriesName
     , "columns" .= columns
     , "points" .= points
     ]
     where
        SeriesData {..} = seriesData

instance FromJSON Series where
    parseJSON (A.Array k) = do
      if V.null k
         then throw $ EmptyException
         else do
             let (Object v) = V.head k
             n <-  v .: "name"
             c <-  v .: "columns"
             p <-  v .: "points"
             return $ Series n (toSD c p)
    parseJSON e = error $ show e

toSD :: [Text] -> [[Value]] -> SeriesData
toSD c p = let col = c
               po  = map (map valToDyn) p
           in SeriesData col po

valToDyn :: Value -> Dyn
valToDyn (String x) = to x
valToDyn (Number x) = case floatingOrInteger x of
                           Left y -> to (y :: Double)
                           Right y -> to (y :: Int)
valToDyn (A.Bool x) = to x
valToDyn Null = to ("null here" :: Text)
valToDyn e = error $ "bad val " ++ show e

data SeriesData = SeriesData
    { columns :: ![Column]
    , points  :: ![[Dyn]]
    } deriving (Show, Eq)

type Column = Text

complexToSeriesData :: Counter -> Complex -> SeriesData
complexToSeriesData _prefixCounter _x = error "complexToSeriesData, not implemented"
  {--  let (c', p') = unzip  x
        prefixCounter' = prefixCounter <> "."
        columns' = map (\y -> prefixCounter' <> y) c'
    in SeriesData columns' [p']
    --}

rebuildComplex :: InfluxDB -> [TaskResult] -> Complex
rebuildComplex db cs = object
    [ "database" .= base db
    , "user"     .= user db
    , "password" .= pass db
    , "points"   .= A.array points
    ]
    where
      points = concatMap convert cs
      convert :: TaskResult -> [Complex]
      convert (TaskResult vHostname vCheck vTriggers vErrorMSG vCheckStatus (Array a)) =
          concatMap (convert . TaskResult vHostname vCheck vTriggers vErrorMSG vCheckStatus) $ V.toList a
      convert (TaskResult vHostname vCheck _ _ vCheckStatus (Object c)) =
          let vName:vTag:_ = map String $ T.splitOn "." $ ctype vCheck
              addition = HM.fromList
                [ ("_check_name_", to $ cname vCheck)
                , ("_check_type_", to $ ctype vCheck)
                , ("_host_name_", to $ vHostname)
                ]
              result = Object $ HM.fromList
                [ ("name", vName)
                , ("tags", object
                    [ "host" .= (to vHostname :: Dyn)
                    , "check_status" .= (String $ T.pack $ show vCheckStatus)
                    , "type" .= vTag
                    ])
                , ("fields", Object $ HM.union c addition)
                ]
          in [result]
      convert (TaskResult vHostname vCheck _ vErrorMSG vCheckStatus Null) =
          let vName:vTag:_ = map String $ T.splitOn "." $ ctype vCheck
              -- TODO add id (see buildCache)
              errorComplex = Object $ HM.fromList
                [ ("_check_name_", to $ cname vCheck)
                , ("_check_type_", to $ ctype vCheck)
                , ("_host_name_", to $ vHostname)
                , ("_error_", String $ T.pack $ show vErrorMSG)
                ]
              result = Object $ HM.fromList
                [ ("name", vName)
                , ("tags", object
                    [ "host" .= (to vHostname :: Dyn)
                    , "check_status" .= (String $ T.pack $ show vCheckStatus)
                    , "type" .= vTag
                    ])
                , ("fields", errorComplex)
                ]
          in [result]
      convert (TaskResult _ _ _ e s c) = error $ "\n\nbad object in rebuildComplex:\nobject: "
        ++ show c
        ++ "\nstatus: " ++ show s
        ++ "\nerror: " ++ show e

saveData :: InfluxDB -> [TaskResult] -> IO ()
saveData db forSave = do
    request' <-  parseUrl $ influxUrl IWrite db
    let series = rebuildComplex db forSave
        request = request'
            { method = "POST"
            , checkStatus = \_ _ _ -> Nothing
            , requestBody = RequestBodyLBS $ encode series
            }
 --    print $ (" ---------- send json" :: String)
 --    putStr $ A.encodePretty series
    response <-  catch (withManager $ \manager -> responseStatus <$> http request manager) catchConduit
    unless (response == ok200 || response == status204 ) $ throw $ DBException $ "Write Influx problem: status = " ++ show response ++ " request = " ++ show request
    return ()
    where
    catchConduit :: HttpException -> IO Status
    catchConduit e = throw $ HTTPException $ "Write Influx problem: http exception = " ++ show e

-- 'http://fixmon:8086/query?pretty=true&db=fixmon' --data-urlencode "q=select * from payments limit 10"
rawRequest :: InfluxDB -> Counter -> Text -> IO Dyn
rawRequest db _c raw = do
    request' <- parseUrl $ influxUrl IQuery db
    let addQueryStr = setQueryString [("db", Just (pack $ base db)), ("u", Just (pack $ user db)), ("p", Just (pack $ pass db)), ("q", Just $ encodeUtf8 raw)]
        request'' = request'
            { method = "GET"
            , checkStatus = \_ _ _ -> Nothing
            }
        request = addQueryStr request''
    print request
    response <- catch (withManager $ \manager -> do
        r <-  http request manager
        result <- responseBody r $$+- consume
        return (responseStatus r, result)
        ) catchConduit
    unless (fst response  == ok200) $ throw $ DBException $ "Influx problem: status = " ++ show response ++ " request = " ++ show request
    let s = (decode . fromChunks . snd $ response :: Maybe Series)
    -- print s
    when (isNothing s) $ throw $ DBException $ "Influx problem: cant parse result"
    return $ undefined -- seriesToDyn c (fromJust s)
    where
    catchConduit :: HttpException -> IO (Status, [ByteString])
    catchConduit e = throw $ DBException $ "Influx problem: http exception = " ++ show e

getData :: InfluxDB -> Text -> Fun -> IO Dyn
-- get last value
getData db vHost (LastFun   c p) = do
    let (pole, addition) = unCounter c
    print c
    print vHost
    xs <- rawRequest db c $ "select "<> pole <>" from " <> vHost <> addition <> " limit " <> ptt p
    case xs of
       --  Array xss -> return $ last xss
         y -> return y
    where
--      counterToIdAndWhere counter =
--          case T.splitOn ":" counter of
--               vId:bucketTypeName:[] -> undefined
getData _ _ _ = error "getData not imlemented"

         {--
getData db vHost (ChangeFun c) = do
    let (pole, addition) = unCounter c
    r <- rawRequest db c $ "select " <> pole <> " from " <> vHost <> addition <> " limit 2"
    case r of
       --  Array (x:y:[]) -> return $ to (x /= y)
         _ -> throw EmptyException
getData db vHost (PrevFun   c) = do
    let (pole, addition) = unCounter c
    rawRequest db "last" $ "select last(" <> pole <> ") from " <> vHost <> addition
getData db vHost (EnvValFun   c) = do
    let (pole, addition) = unCounter c
    xs <- rawRequest db c $ "select "<> pole <>" from " <> vHost <> addition <> " limit 1"
    case xs of
       --  Array xss -> return $ last xss
         y -> return y
getData db vHost (AvgFun    c p) = do
    let (pole, addition) = unCounter c
    rawRequest db "mean" $ "select mean(" <> pole <> ") from " <> vHost <> " group by time(" <> pt p <> ") where time > now() - " <> pt p <> withAnd addition
getData db vHost (MinFun    c p) = do
    let (pole, addition) = unCounter c
    rawRequest db "min" $ "select min(" <> pole <> ") from " <> vHost <> " group by time(" <> pt p <> ") where time > now() - " <> pt p <> withAnd addition
getData db vHost (MaxFun    c p) = do
    let (pole, addition) = unCounter c
    rawRequest db "max" $ "select max(" <> pole <> ") from " <> vHost <> " group by time(" <> pt p <> ") where time > now() - " <> pt p <> withAnd addition
getData db vHost (NoDataFun c p) = do
    let (pole, addition) = unCounter c
    r <- try $ rawRequest db c $ "select " <> pole <> " from " <> vHost <> " where time > now() - " <> pt p <> withAnd addition <> " limit 1"
    case r of
         Right _ -> return $ to False
         Left EmptyException -> return $ to True
         Left e -> throw e
--}

withAnd :: Text -> Text
withAnd "" = ""
withAnd x = " and " <> T.drop 6 x

unCounter :: Counter -> (Text, Text)
unCounter _x = undefined {-- case T.splitOn ":" x of
                             [a] -> (a, T.empty)
                             [a, b] -> (b, " where " <> T.dropWhileEnd (/= '.') b <> "id = '" <> a <> "' ")
                             _ -> throw $ DBException "bad counter"
--}
pt :: Period Int -> Text
pt x = (T.pack . show $ (fromIntegral $ un x :: Double)/1000000) <> "s"

ptt :: Period Int -> Text
ptt  = T.pack . show . un

seriesToDyn :: Text -> Series -> Dyn
seriesToDyn c s = let col = columns . seriesData $ s
                      poi = points . seriesData $ s
                      mapped = catMaybes $ map (\x -> lookup c (zip col x)) poi
                  in case mapped of
                          [] -> throw $ TypeException $ "Influx problem: cant found result for counter " ++ show c
                          [x] -> x
                        --  xs -> Array xs
                          _ -> error "ops series to dyn"




-- curl -G 'http://localhost:8086/db/fixmon/series?u=fixmon&p=fixmon' --data-urlencode "q=select status from localhost limit 1"


