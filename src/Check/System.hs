{-# LANGUAGE OverloadedStrings #-}
module Check.System where

import Check
import Types

import Data.Map (fromList)
import Data.Text (pack, Text)
import Data.Text.IO (readFile)
import Data.Attoparsec.Text
import Control.Applicative
import System.Cron (daily)
import Data.Monoid ((<>))

import Network.BSD (getHostName)
import Prelude hiding (readFile)

data System = HostName
            | Uptime
            | Boottime
            | CpuIntr
            | CpuLoad 
            | CpuNum 
            | CpuSwitches
            | CpuUtil 
            | LocalTime
            deriving (Show)

instance Checkable System where
    route HostName = ("system.hostname", doHostname)
    route Uptime   = ("system.uptime",   doUptime)
    route Boottime = ("system.boottime",   doCheck)
    route CpuIntr  = ("system.cpu.intr",   doCheck)
    route CpuLoad  = ("system.cpu.load",   doCheck)
    route CpuNum  =  ("system.cpu.num",   doCheck)
    route CpuSwitches  =  ("system.cpu.switches",   doCheck)
    route CpuUtil  =  ("system.cpu.util",   doCheck)
    route LocalTime  =  ("system.localtime",   doCheck)

    describe HostName = []
    describe Uptime   = []
    describe Boottime   = []
    describe LocalTime   = []
    describe CpuIntr = []
    describe CpuLoad = []
    describe CpuNum = []
    describe CpuSwitches = []
    describe CpuUtil = []

    isCorrect ch h = undefined

doHostname (Check _ _ "system.hostname" _) = do
    h <- getHostName
    return $ Complex $ fromList [ ( "system.hostname", Any $ Text (pack h)) ]

-- "3023604.41 11190196.16\n"

uptimeFile = "/proc/uptime"

doUptime (Check _ _ "system.uptime" _) = do
    Done _ (up, idle) <-  parse parserUptime <$> readFile uptimeFile
    return $ Complex $ fromList [ ("system.uptime.up", Any $ Text (timeToPeriod up))
                                , ("system.uptime.idle", Any $ Text (timeToPeriod idle))
                                ]
    where
       timeToPeriod x = 
         let i = truncate x
             days = i `div` 86400
             hours = (i `mod` 86400) `div` 3600
             mins = (i `mod` 3600) `div` 60
             showD = if days /= 0 then (pack (show days)) <> " days " else ""
             showH = if hours /= 0 then (pack (show hours)) <> " hours " else ""
             showM = if mins /= 0 then (pack (show mins)) <> " minunes" else ""
         in "up " <> showD <> showH <> showM
       parserUptime = (,) <$> rational <* space <*> rational <* endOfLine


testUptime = Check (CheckName "uptime") (Cron daily) "system.uptime" (fromList [])


doCheck :: Check -> IO Complex
doCheck = undefined
{--
class Checkable a where
    route :: a -> (Name, Check -> IO Complex)
    describe :: a -> [(Field, Required, Description)]
    isCorrect :: Check -> a -> Either Text Check
    --}
