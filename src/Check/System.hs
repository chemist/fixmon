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
import Data.Time.Clock

import Network.BSD (getHostName)
import Prelude hiding (readFile, takeWhile)

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
    route Boottime = ("system.boottime",   doBootTime)
    route CpuIntr  = ("system.cpu.intr",   doCpuIntr)
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

    isCorrect ch@(Check _ _ "system.hostname" _) HostName = Right ch
    isCorrect ch@(Check _ _ "system.uptime" _) Uptime = Right ch
    isCorrect ch@(Check _ _ "system.boottime" _) Boottime = Right ch
    isCorrect ch@(Check _ _ "system.cpu.intr" _) CpuIntr = Right ch
    isCorrect ch@(Check _ _ "system.cpu.load" _) CpuLoad = Right ch
    isCorrect ch@(Check _ _ "system.cpu.num" _) CpuNum = Right ch
    isCorrect ch@(Check _ _ "system.cpu.switches" _) CpuSwitches = Right ch
    isCorrect ch@(Check _ _ "system.cpu.util" _) CpuUtil = Right ch
    isCorrect ch@(Check _ _ "system.localtime" _) LocalTime = Right ch
    isCorrect _ _ = Left "oops, check is not correct"

---------------------------------- linux checks --------------------------------------
doHostname (Check _ _ "system.hostname" _) = do
    h <- getHostName
    return $ Complex $ fromList [ ( "system.hostname", Any $ Text (pack h)) ]
--------------------------------------------------------------------------------------
    
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
--------------------------------------------------------------------------------------

doBootTime (Check _ _ "system.boottime" _) = do
    Done _ (up, _) <- parse parserUptime <$> readFile uptimeFile
    now <- getCurrentTime
    let boot = addUTCTime (-diff) now
        diff = toEnum . fromEnum . secondsToDiffTime . truncate $ up
    return $ Complex $ fromList [ ("system.boottime", Any . UTC $ boot)]
--------------------------------------------------------------------------------------

doCpuIntr (Check _ _ "system.cpu.intr" _) = do
    undefined

spaces = takeWhile1 isHorizontalSpace

spaces' = takeWhile isHorizontalSpace

cpuN = takeWhile1 (inClass "a-zA-Z0-9")

data Cpu = Cpu [Text] deriving (Show, Eq)
data Interrupt = Interrupt Text [Int] (Text) deriving (Show, Eq)

parserInterruptsCPU = Cpu <$> manyTill' (spaces *> cpuN) (spaces *> endOfLine)
parserInterruptsLine = Interrupt <$> (spaces' *> cpuN <* char ':') <*> (many' (spaces *> decimal)) <*> (spaces' *> takeTill isEndOfLine <* endOfLine)

parserInterrupts = do
    c <- parserInterruptsCPU
    l <- many parserInterruptsLine
    endOfInput
    return (c,l)

doCheck :: Check -> IO Complex
doCheck = undefined
{--
class Checkable a where
    route :: a -> (Name, Check -> IO Complex)
    describe :: a -> [(Field, Required, Description)]
    isCorrect :: Check -> a -> Either Text Check
    --}
