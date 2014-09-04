{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Storage
( storage
, defStorage
, saveResult
)
where


import           Control.Distributed.Process                         (Process,
                                                                      liftIO,
                                                                      say, spawnLocal)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson                                          as A
import           Data.Maybe                                          (fromMaybe)
import           Data.Monoid                                         ((<>))
import           Data.Text                                           hiding
                                                                      (map,
                                                                      pack)

import           Data.ByteString.Char8                               (pack)
import           Network.HTTP.Conduit                                hiding
                                                                      (host,
                                                                      port)

import           Types                                               hiding
                                                                      (config)


---------------------------------------------------------------------------------------------------
-- public
---------------------------------------------------------------------------------------------------

saveResult :: (Hostname, Complex) -> Process ()
saveResult = cast (Registered "storage")

defStorage :: Process ()
defStorage = storage Nothing Nothing Nothing Nothing Nothing Nothing

storage :: Maybe DB -> Maybe User -> Maybe Password -> Maybe Host -> Maybe Port -> Maybe EnableSSL -> Process ()
storage d u p h po s = serve conf initServer server
  where
    conf = InfluxConfig { db   = fromMaybe (db defConf) d
                        , user = fromMaybe (user defConf) u
                        , pass = fromMaybe (pass defConf) p
                        , host = fromMaybe (host defConf) h
                        , port = fromMaybe (port defConf) po
                        , ssl  = fromMaybe (ssl defConf) s
                        }

--------------------------------------------------------------------------------------------------
-- private
--------------------------------------------------------------------------------------------------

-- doCron :: Process ()
-- doCron = cast (Registered "cron") MinuteMessage

defDelay :: Delay
defDelay = Delay $ seconds 1

data ST = ST
  { config :: {-# UNPACK #-}  !InfluxConfig
  , queue  :: {-# UNPACK #-}  ![(Hostname, Complex)]
  }

initServer :: InitHandler InfluxConfig ST
initServer conf = do
    say "start storage"
    return $! InitOk (ST conf []) defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ saveToQueue ]
    , timeoutHandler = \st _ -> do
        !_ <- spawnLocal $! saveAll st
        timeoutAfter_ defDelay (st { queue = [] })
    , infoHandlers = []
    , unhandledMessagePolicy = Log
    }

saveAll :: ST -> Process ()
saveAll st = do
    request' <-  liftIO $ parseUrl $ influxUrl $ config st
    let conf = config st
        addQueryStr = setQueryString [("u", Just (pack $ user conf)), ("p", Just (pack $ pass conf))]
        series = map toSeries (queue st)
        request'' = request'
            { method = "POST"
            , checkStatus = \_ _ _ -> Nothing
            , requestBody = RequestBodyLBS $ encode series
            }
        request = addQueryStr request''
    unless (series == []) $ do
        void . liftIO $ withManager $ \manager -> do
            !response <-  request `seq` http request manager
            return $! responseStatus response
        return ()


saveToQueue :: Dispatcher ST
saveToQueue = handleCast $ \st (message :: (Hostname, Complex)) -> continue $! st { queue = message : queue st }


--------------------------------------------------------------------------------------------------
-- internal
--------------------------------------------------------------------------------------------------

type DB = String
type User = String
type Password = String
type Host = String
type Port = Int
type EnableSSL = Bool

data InfluxConfig = InfluxConfig
  { db   :: {-# UNPACK #-}  !DB
  , user :: {-# UNPACK #-}  !User
  , pass :: {-# UNPACK #-}  !Password
  , host :: {-# UNPACK #-}  !Host
  , port :: {-# UNPACK #-}  !Port
  , ssl  :: {-# UNPACK #-}  !EnableSSL
  } deriving Show

defConf :: InfluxConfig
defConf = InfluxConfig "fixmon" "fixmon" "fixmon" "localhost" 8086 False

influxUrl :: InfluxConfig -> String
influxUrl conf = let scheme = if ssl conf
                                 then "https://"
                                 else "http://"
                     port' = show . port $ conf
                 in scheme <> host conf <> ":" <> port' <> "/db/" <> db conf <> "/series"
data Series = Series
    { seriesName :: {-# UNPACK #-}  !Text
    , seriesData :: {-# UNPACK #-}  !SeriesData
    } deriving (Show, Eq)

instance ToJSON Series where
   toJSON Series {..} = A.object
     [ "name" .= seriesName
     , "columns" .= columns
     , "points" .= points
     ]
     where
        SeriesData {..} = seriesData

data SeriesData = SeriesData
    { columns :: {-# UNPACK #-}  ![Column]
    , points  :: {-# UNPACK #-}  ![[Dyn]]
    } deriving (Show, Eq)

type Column = Counter

complexToSeriesData :: Complex -> SeriesData
complexToSeriesData (Complex x) = let (c', p') = unzip x
                                  in SeriesData c' [p']

toSeries :: (Hostname, Complex) -> Series
toSeries ((Hostname n), c) = Series n (complexToSeriesData c)

{--
c = Complex (fromList [("system.hostname",Any $ Text "limbo-air"), ("system.loadavg", Any $ Int 10)])

cc :: [Series]
cc = map (\x -> Series "newlimbo" (complexToSeriesData $ Complex (fromList [("system.hostname", Any $ Text "limbo-air"), ("system.loadavg", Any $ Int x)]))) $ [1 .. 5000]

s = Series "limbo" (complexToSeriesData c)

-- series :: Value
-- series = toJSON [ 1::Int, 2,3 ]
--}
