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
                                                                      spawnLocal,
                                                                      say)
import           Control.Distributed.Process.Platform                (Recipient (..))
import           Control.Distributed.Process.Platform.ManagedProcess
import           Control.Distributed.Process.Platform.Time
import           Control.Monad

import           Control.Lens                                        hiding
                                                                      ((.=))
import           Data.Aeson
import qualified Data.Aeson                                          as A
import           Data.Map                                            hiding
                                                                      (map)
import           Data.Maybe                                          (fromMaybe)
import           Data.Monoid                                         ((<>))
import           Data.Text                                           hiding
                                                                      (map)
import           Network.Wreq

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
  { config :: InfluxConfig
  , pool   :: String
  , queue  :: [(Hostname, Complex)]
  }

initServer :: InitHandler InfluxConfig ST
initServer conf = do
    say "start storage"
    return $ InitOk (ST conf "test" []) defDelay

server :: ProcessDefinition ST
server = defaultProcess
    { apiHandlers = [ saveToQueue ]
    , timeoutHandler = \st _ -> do
        spawnLocal $ saveAll st
        timeoutAfter_ defDelay (st { queue = [] })
    , infoHandlers = []
    }

saveAll :: ST -> Process ()
saveAll st = do
    let opts = defaults & param "u" .~ [pack . user $ defConf] & param "p" .~ [pack . pass $ defConf]
        conf = config st
        series = map toSeries (queue st)
    unless (series == []) $ do
        r <- liftIO $ postWith opts (influxUrl conf) (toJSON series)
        say $ show r
        return ()


saveToQueue :: Dispatcher ST
saveToQueue = handleCast $ \st (message :: (Hostname, Complex)) -> continue $ st { queue = message : queue st }


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
  { db   :: !DB
  , user :: !User
  , pass :: !Password
  , host :: !Host
  , port :: !Port
  , ssl  :: !EnableSSL
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
    { seriesName :: Text
    , seriesData :: SeriesData
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
    { columns :: [Column]
    , points  :: [[Any]]
    } deriving (Show, Eq)

type Column = Text

complexToSeriesData :: Complex -> SeriesData
complexToSeriesData (Complex x) = let (c', p') = unzip  $ toList x
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
