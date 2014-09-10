{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Storage.InfluxDB where

import           Control.Monad         (unless, void)
import           Data.Aeson (ToJSON(..), object, (.=), encode)
import           Data.ByteString.Char8 (pack)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import           Network.HTTP.Conduit  hiding (host, port)
import           Types

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
  { db   :: !DB
  , user :: !User
  , pass :: !Password
  , host :: !Host
  , port :: !Port
  , ssl  :: !EnableSSL
  } deriving Show

defConf :: InfluxDB
defConf = InfluxDB "fixmon" "fixmon" "fixmon" "localhost" 8086 False

influxUrl :: InfluxDB -> String
influxUrl conf = let scheme = if ssl conf
                                 then "https://"
                                 else "http://"
                     port' = show . port $ conf
                 in scheme <> host conf <> ":" <> port' <> "/db/" <> db conf <> "/series"

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
        void $ withManager $ \manager -> do
            response <-  request `seq` http request manager
            return $! responseStatus response
        return ()


