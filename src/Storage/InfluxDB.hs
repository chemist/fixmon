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
import           Types hiding (Status)
import           Control.Applicative ((<$>))
import           Control.Exception (try, throw, catch)
import           Data.Scientific (floatingOrInteger)
import Data.Maybe
import Data.Typeable (TypeRep)
import qualified Prelude as Prelude
import Prelude
-- import Debug.Trace

instance Database InfluxDB where
    getData = get
    saveData = save
    config = defConf



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

defConf :: InfluxDB
defConf = InfluxDB "fixmon" "fixmon" "fixmon" "localhost" 8086 False

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
valToDyn (Bool x) = toDyn x
valToDyn Null = toDyn ("null here" :: Text)
valToDyn e = error $ "bad val " ++ show e

data SeriesData = SeriesData
    { columns :: ![Column]
    , points  :: ![[Dyn]]
    } deriving (Show, Eq)

type Column = Counter

complexToSeriesData :: Complex -> SeriesData
complexToSeriesData (Complex x) = let (c', p') = unzip x
                                  in SeriesData c' [p']

toSeries :: (Hostname, [Complex]) -> [Series]
toSeries (Hostname n, c) = map (\x -> Series n (complexToSeriesData x)) c

save :: InfluxDB -> [(Hostname, [Complex])] -> IO ()
save db forSave = do
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

toWhere :: Where -> Text
toWhere = Prelude.foldl (\t (c,d) -> t <> unCounter c <> " = " <> T.pack (Prelude.show  d)) " where "  

get :: InfluxDB -> Table -> Where -> Fun -> IO Dyn
get db t w (ChangeFun c) = do
    r <- rawRequest db c $ "select " <> unCounter c <> " from " <> unTable t <> " limit 2"
    case r of
         DynList (x:y:[]) -> return $ toDyn (x /= y)
         _ -> throw EmptyException
get db t w (PrevFun   c) = rawRequest db (Counter "last") ("select last(" <> unCounter c <> ") from " <> unTable t)
get db t w (LastFun   c p) = do
    xs <- rawRequest db c ("select "<> unCounter c <>" from " <> unTable t <> " limit " <> ptt p)
    case xs of
         DynList xss -> return $ last xss
         y -> return y
get db t w (AvgFun    c p) = do
    typeR <- counterType db t c
    when (typeR /= iType && typeR /= dType) $ throw $ TypeException "Influx problem: avg function, counter value must be number"
    rawRequest db (Counter "mean") ("select mean(" <> unCounter c <> ") from " <> unTable t <> " group by time(" <> pt p <> ") where time > now() - " <> pt p)
get db t w (MinFun    c p) = do
    typeR <- counterType db t c
    when (typeR /= iType && typeR /= dType) $ throw $ TypeException "Influx problem: min function, counter value must be number"
    rawRequest db (Counter "min") ("select min(" <> unCounter c <> ") from " <> unTable t <> " group by time(" <> pt p <> ") where time > now() - " <> pt p)
get db t w (MaxFun    c p) = do
    typeR <- counterType db t c
    when (typeR /= iType && typeR /= dType) $ throw $ TypeException "Influx problem: max function, counter value must be number"
    rawRequest db (Counter "max") ("select max(" <> unCounter c <> ") from " <> unTable t <> " group by time(" <> pt p <> ") where time > now() - " <> pt p)
get db t w (NoDataFun c p) = do
    r <- try $ rawRequest db c ("select " <> unCounter c <> " from " <> unTable t <> " where time > now() - " <> pt p <> " limit 1") 
    case r of
         Right _ -> return $ toDyn False
         Left EmptyException -> return $ toDyn True
         Left e -> throw e

counterType :: InfluxDB -> Table -> Counter -> IO TypeRep
counterType db t c = do
    r <- rawRequest db (Counter "last") ("select last(" <> unCounter c <> ") from " <> unTable t)
    return $ dynTypeRep r

-- min  SELECT MIN(status) FROM localhost group by time(24h) where time > now() - 24h

unTable :: Table -> Text
unTable (Table x) = x
unCounter :: Counter -> Text
unCounter (Counter x) = x

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
    return $ seriesToDyn c $ fromJust s
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


