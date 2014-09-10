{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Storage.InfluxDB where

import           Control.Monad         (unless)
import           Data.Aeson (ToJSON(..), object, (.=), encode)
import           Data.ByteString.Char8 (pack)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import           Network.HTTP.Conduit  hiding (host, port)
import           Network.HTTP.Types.Status
import           Types hiding (Status)
import           Control.Applicative ((<$>))
import           Control.Exception (throw, catch)

instance Database InfluxDB where
    getData = undefined
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

data SeriesData = SeriesData
    { columns :: ![Column]
    , points  :: ![[Dyn]]
    } deriving (Show, Eq)

type Column = Counter

complexToSeriesData :: Complex -> SeriesData
complexToSeriesData (Complex x) = let (c', p') = unzip x
                                  in SeriesData c' [p']

toSeries :: (Hostname, Complex) -> Series
toSeries ((Hostname n), c) = Series n (complexToSeriesData c)


save :: InfluxDB -> [(Hostname, Complex)] -> IO ()
save db forSave = do
    request' <-  parseUrl $ influxUrl db
    let addQueryStr = setQueryString [("u", Just (pack $ user db)), ("p", Just (pack $ pass db))]
        series = map toSeries forSave
        request'' = request'
            { method = "POST"
            , checkStatus = \_ _ _ -> Nothing
            , requestBody = RequestBodyLBS $ encode series
            }
        request = addQueryStr request''
    unless (series == []) $ do
        response <-  catch (withManager $ \manager -> responseStatus <$> http request manager) catchConduit
        unless (response == ok200) $ throw $ DBException $ "Influx problem: status = " ++ show response ++ " request = " ++ show request 
    return ()
    where
    catchConduit :: HttpException -> IO Status
    catchConduit e = throw $ DBException $ "Influx problem: http exception = " ++ show e

get :: InfluxDB -> Table -> Fun -> IO Dyn
get _db _t (ChangeFun _c) = undefined
get _db _t (PrevFun   _c) = undefined
get _db _t (LastFun   _c _p) = undefined
get _db _t (AvgFun    _c _p) = undefined
get _db _t (MinFun    _c _p) = undefined
get _db _t (MaxFun    _c _p) = undefined
get _db _t (NoDataFun _c _p) = undefined

-- curl -G 'http://localhost:8086/db/fixmon/series?u=fixmon&p=fixmon' --data-urlencode "q=select status from localhost limit 1"


