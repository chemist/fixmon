{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Storage.InfluxDB where

import           Control.Monad         (unless, when)
import           Data.Aeson (ToJSON(..), object, (.=), encode, FromJSON(..), (.:), Value(..), decode)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import           Data.ByteString.Char8 (pack, ByteString)
import           Data.ByteString.Lazy (fromChunks)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text as T
import           Data.Text.Encoding    (encodeUtf8)
import           Network.HTTP.Conduit  hiding (host, port)
import           Data.Conduit.List (consume)
import           Data.Conduit (($$+-))
import           Network.HTTP.Types.Status
import           Control.Applicative ((<$>))
import           Control.Exception (try, throw, catch)
import           Data.Scientific (floatingOrInteger)
import Data.Maybe
import Types.Shared hiding (Status)
import Types.Dynamic
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
config = InfluxDB "fixmon" "fixmon" "fixmon" "localhost" 8086 False

influxUrl :: InfluxDB -> String
influxUrl db = let scheme = if ssl db
                                 then "https://"
                                 else "http://"
                   port' = show . port $ db
               in scheme <> host db <> ":" <> port' <> "/db/" <> base db <> "/series"

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
toSD c p = let col = map Counter c
               po  = map (map valToDyn) p
           in SeriesData col po

valToDyn :: Value -> Dyn
valToDyn (String x) = toDyn x
valToDyn (Number x) = case floatingOrInteger x of
                           Left y -> toDyn (y :: Double)
                           Right y -> toDyn (y :: Int)
valToDyn (A.Bool x) = toDyn x
valToDyn Null = toDyn ("null here" :: Text)
valToDyn e = error $ "bad val " ++ show e

data SeriesData = SeriesData
    { columns :: ![Column]
    , points  :: ![[Dyn]]
    } deriving (Show, Eq)

type Column = Counter

complexToSeriesData :: Counter -> Complex -> SeriesData
complexToSeriesData prefixCounter (Complex x) = 
    let (c', p') = unzip x
        prefixCounter' = prefixCounter <> "."
        columns' = map (\y -> prefixCounter' <> y) c'
    in SeriesData columns' [p']

toSeries :: (Hostname, Counter, [Complex]) -> [Series]
toSeries (Hostname n, prefixCounter, c) = map (\x -> Series n (complexToSeriesData prefixCounter x)) c

saveData :: InfluxDB -> [(Hostname, Counter, [Complex])] -> IO ()
saveData db forSave = do
    request' <-  parseUrl $ influxUrl db
    let addQueryStr = setQueryString [("u", Just (pack $ user db)), ("p", Just (pack $ pass db))]
        series = concatMap toSeries forSave
        request'' = request'
            { method = "POST"
            , checkStatus = \_ _ _ -> Nothing
            , requestBody = RequestBodyLBS $ encode series
            }
        request = addQueryStr request''
    -- print $ encode series
    unless (null series) $ do
        response <-  catch (withManager $ \manager -> responseStatus <$> http request manager) catchConduit
        unless (response == ok200) $ throw $ DBException $ "Influx problem: status = " ++ show response ++ " request = " ++ show request 
    return ()
    where
    catchConduit :: HttpException -> IO Status
    catchConduit e = throw $ HTTPException $ "Influx problem: http exception = " ++ show e

getData :: InfluxDB -> Table -> Fun -> IO Dyn
getData db t (ChangeFun c) = do
    let (pole, addition) = unCounter c
    r <- rawRequest db c $ "select " <> pole <> " from " <> unTable t <> addition <> " limit 2"
    case r of
         DynList (x:y:[]) -> return $ toDyn (x /= y)
         _ -> throw EmptyException
getData db t (PrevFun   c) = do
    let (pole, addition) = unCounter c
    rawRequest db (Counter "last") $ "select last(" <> pole <> ") from " <> unTable t <> addition
getData db t (LastFun   c p) = do
    let (pole, addition) = unCounter c
    xs <- rawRequest db c $ "select "<> pole <>" from " <> unTable t <> addition <> " limit " <> ptt p
    case xs of
         DynList xss -> return $ last xss
         y -> return y
getData db t (EnvValFun   c) = do
    let (pole, addition) = unCounter c
    xs <- rawRequest db c $ "select "<> pole <>" from " <> unTable t <> addition <> " limit 1" 
    case xs of
         DynList xss -> return $ last xss
         y -> return y
getData db t (AvgFun    c p) = do
    let (pole, addition) = unCounter c
    rawRequest db (Counter "mean") $ "select mean(" <> pole <> ") from " <> unTable t <> " group by time(" <> pt p <> ") where time > now() - " <> pt p <> withAnd addition
getData db t (MinFun    c p) = do
    let (pole, addition) = unCounter c
    rawRequest db (Counter "min") $ "select min(" <> pole <> ") from " <> unTable t <> " group by time(" <> pt p <> ") where time > now() - " <> pt p <> withAnd addition
getData db t (MaxFun    c p) = do
    let (pole, addition) = unCounter c
    rawRequest db (Counter "max") $ "select max(" <> pole <> ") from " <> unTable t <> " group by time(" <> pt p <> ") where time > now() - " <> pt p <> withAnd addition
getData db t (NoDataFun c p) = do
    let (pole, addition) = unCounter c
    r <- try $ rawRequest db c $ "select " <> pole <> " from " <> unTable t <> " where time > now() - " <> pt p <> withAnd addition <> " limit 1" 
    case r of
         Right _ -> return $ toDyn False
         Left EmptyException -> return $ toDyn True
         Left e -> throw e

unTable :: Table -> Text
unTable (Table x) = x

withAnd :: Text -> Text
withAnd "" = ""
withAnd x = " and " <> T.drop 6 x

unCounter :: Counter -> (Text, Text)
unCounter (Counter x) = case T.splitOn ":" x of
                             [a] -> (a, T.empty)
                             [a, b] -> (b, " where " <> T.dropWhileEnd (/= '.') b <> "id = '" <> a <> "' ")
                             _ -> throw $ DBException "bad counter"

pt :: Period Int -> Text
pt x = (T.pack . show $ (fromIntegral $ un x :: Double)/1000000) <> "s"

ptt :: Period Int -> Text
ptt  = T.pack . show . un 

rawRequest :: InfluxDB -> Counter -> Text -> IO Dyn
rawRequest db c raw = do
    request' <- parseUrl $ influxUrl db
    let addQueryStr = setQueryString [("u", Just (pack $ user db)), ("p", Just (pack $ pass db)), ("q", Just $ encodeUtf8 raw)]
        request'' = request'
            { method = "GET"
            , checkStatus = \_ _ _ -> Nothing
            }
        request = addQueryStr request''
    -- print request
    response <- catch (withManager $ \manager -> do
        r <-  http request manager
        result <- responseBody r $$+- consume
        return (responseStatus r, result)
        ) catchConduit
    unless (fst response  == ok200) $ throw $ DBException $ "Influx problem: status = " ++ show response ++ " request = " ++ show request 
    let s = (decode . fromChunks . snd $ response :: Maybe Series)
    -- print s
    when (isNothing s) $ throw $ DBException $ "Influx problem: cant parse result"
    return $ seriesToDyn (Counter . fst . unCounter $ c) (fromJust s)
    where
    catchConduit :: HttpException -> IO (Status, [ByteString])
    catchConduit e = throw $ DBException $ "Influx problem: http exception = " ++ show e

seriesToDyn :: Counter -> Series -> Dyn
seriesToDyn c s = let col = columns . seriesData $ s
                      poi = points . seriesData $ s
                      mapped = catMaybes $ map (\x -> lookup c (zip col x)) poi
                  in case mapped of
                          [] -> throw $ TypeException $ "Influx problem: cant found result for counter " ++ show c
                          [x] -> x
                          xs -> DynList xs

                              


-- curl -G 'http://localhost:8086/db/fixmon/series?u=fixmon&p=fixmon' --data-urlencode "q=select status from localhost limit 1"


