{-# LANGUAGE OverloadedStrings #-}
module Check.System where

import Check
import Types

import Data.Map (fromList)
import Data.Text (pack, Text, toLower, isPrefixOf)
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
            | CpuInfo 
            | CpuSwitches
            | CpuUtil 
            | LocalTime
            deriving (Show)

instance Checkable System where
    route HostName = ("system.hostname", doHostname)
    route Uptime   = ("system.uptime",   doUptime)
    route Boottime = ("system.boottime",   doBootTime)
    route CpuIntr  = ("system.cpu.intr",   doCpuIntr)
    route CpuLoad  = ("system.cpu.loadavg",   doCpuLoad)
    route CpuInfo  =  ("system.cpu.info",   doCpuInfo)
    route CpuSwitches  =  ("system.cpu.switches",   doCheck)
    route CpuUtil  =  ("system.cpu.util",   doCheck)
    route LocalTime  =  ("system.localtime",   doCheck)

    describe HostName = []
    describe Uptime   = []
    describe Boottime   = []
    describe LocalTime   = []
    describe CpuIntr = []
    describe CpuLoad = []
    describe CpuInfo = []
    describe CpuSwitches = []
    describe CpuUtil = []

    isCorrect ch@(Check _ _ "system.hostname" _) HostName = Right ch
    isCorrect ch@(Check _ _ "system.uptime" _) Uptime = Right ch
    isCorrect ch@(Check _ _ "system.boottime" _) Boottime = Right ch
    isCorrect ch@(Check _ _ "system.cpu.intr" _) CpuIntr = Right ch
    isCorrect ch@(Check _ _ "system.cpu.loadavg" _) CpuLoad = Right ch
    isCorrect ch@(Check _ _ "system.cpu.info" _) CpuInfo = Right ch
    isCorrect ch@(Check _ _ "system.cpu.switches" _) CpuSwitches = Right ch
    isCorrect ch@(Check _ _ "system.cpu.util" _) CpuUtil = Right ch
    isCorrect ch@(Check _ _ "system.localtime" _) LocalTime = Right ch
    isCorrect _ _ = Left "oops, check is not correct"

---------------------------------- linux checks --------------------------------------
doHostname :: Check -> IO Complex
doHostname (Check _ _ "system.hostname" _) = do
    h <- getHostName
    return $ Complex $ fromList [ ( "system.hostname", Any $ Text (pack h)) ]
doHostname _ = undefined
--------------------------------------------------------------------------------------
    
-- "3023604.41 11190196.16\n"
uptimeFile :: String
uptimeFile = "/proc/uptime"

doUptime :: Check -> IO Complex
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
             showD = if days /= (0 :: Integer) then (pack (show days)) <> " days " else ""
             showH = if hours /= 0 then (pack (show hours)) <> " hours " else ""
             showM = if mins /= 0 then (pack (show mins)) <> " minunes" else ""
         in "up " <> showD <> showH <> showM
doUptime _ = undefined

parserUptime :: Parser (Double, Double)
parserUptime = (,) <$> rational <* space <*> rational <* endOfLine

testUptime :: Check
testUptime = Check (CheckName "uptime") (Cron daily) "system.uptime" (fromList [])
--------------------------------------------------------------------------------------
doBootTime :: Check -> IO Complex
doBootTime (Check _ _ "system.boottime" _) = do
    Done _ (up, _) <- parse parserUptime <$> readFile uptimeFile
    now <- getCurrentTime
    let boot = addUTCTime (-diff) now
        diff = toEnum . fromEnum . secondsToDiffTime . truncate $ up
    return $ Complex $ fromList [ ("system.boottime", Any . UTC $ boot)]
doBootTime _ = undefined
--------------------------------------------------------------------------------------

testIntr :: Check
testIntr = Check (CheckName "interrupts") (Cron daily) "system.cpu.intr" (fromList [])

-- intrFile = "interrupts"
intrFile :: String
intrFile = "/proc/interrupts"

doCpuIntr :: Check -> IO Complex
doCpuIntr (Check _ _ "system.cpu.intr" _) = do
    Right (c, i) <- parseOnly parserInterrupts <$> readFile intrFile
    return $ Complex $ fromList $ mkInterrupts (c,i)
doCpuIntr _ = undefined
    
mkInterrupts :: (Cpu, [Interrupt]) -> [(Text, Any)]
mkInterrupts (Cpu c, i) = 
  let lowerCpuN = map toLower c
      addAll xs = ("allcpu", sum $ map snd xs) : xs
      mk (Interrupt t ns _) =  map (\(x, y) -> ("system.cpu.intr." <> x <> "." <> toLower t, toAny y)) $ addAll $ zip lowerCpuN ns
      intr = concatMap mk i
  in interrupsAll intr : intr

interrupsAll :: [(Text, Any)] -> (Text, Any)
interrupsAll xs = 
  let onlyAll = filter (\(x, _) -> isPrefixOf "system.cpu.intr.allcpu" x) xs
  in ("system.cpu.intr.total", Any . Int . sum $ map (\(_, x) -> unAny x)  onlyAll)

spaces :: Parser ()
spaces = skipWhile isHorizontalSpace

cpuN :: Parser Text
cpuN = takeWhile1 (inClass "a-zA-Z0-9")

data Cpu = Cpu [Text] deriving (Show, Eq)
data Interrupt = Interrupt Text [Int] Text deriving (Show, Eq)

parserInterruptsCPU :: Parser Cpu
parserInterruptsCPU = Cpu <$> manyTill' (spaces *> cpuN) (spaces *> endOfLine)

parserInterruptsLine :: Parser Interrupt
parserInterruptsLine = Interrupt <$> (spaces *> cpuN <* char ':') <*> (spaces *> decimal `sepBy` spaces) <*> (spaces *> takeTill isEndOfLine)

parserInterrupts :: Parser (Cpu, [Interrupt])
parserInterrupts = (,) <$> parserInterruptsCPU <*> (parserInterruptsLine `sepBy` endOfLine)
--------------------------------------------------------------------------------------

loadavgFile :: String
loadavgFile = "/proc/loadavg"

testLoadAvg :: Check
testLoadAvg = Check (CheckName "loadavg") (Cron daily) "system.cpu.loadavg" (fromList [])

doCpuLoad :: Check -> IO Complex
doCpuLoad (Check _ _ "system.cpu.loadavg" _) = do
    Right (x,y,z) <- parseOnly parserLoadavg <$> readFile loadavgFile
    return . Complex . fromList $ [ ("system.cpu.la1", Any . Double $ x)
                                  , ("system.cpu.la5", Any . Double $ y)
                                  , ("system.cpu.la15", Any . Double $ z)
                                  ]
doCpuLoad _ = undefined

parserLoadavg :: Parser (Double, Double, Double)
parserLoadavg = (,,) <$> rational <* space <*> rational <* space <*> rational 
--------------------------------------------------------------------------------------

cpuFile :: String
cpuFile = "cpuinfo"
-- cpuFile = "/proc/cpuinfo"

testCpuInfo :: Check
testCpuInfo = Check (CheckName "cpuinfo") (Cron daily) "system.cpu.info" (fromList [])

doCpuInfo :: Check -> IO Complex
doCpuInfo (Check _ _ "system.cpu.info" _) = do
    Right cpus <- parseOnly (parserCpuInf `sepBy` char '\n') <$> readFile cpuFile
    let one = head cpus
    return . Complex . fromList $ [ ("system.cpu.info.num", Any . Int $ length cpus)
                                  , ("system.cpu.info.vendor_id", Any . Text $ vendorId  one)
                                  , ("system.cpu.info.cpu_family", Any . Text $ cpuFamily one)
                                  , ("system.cpu.info.model", Any . Int $ model one)
                                  , ("system.cpu.info.model_name", Any . Text $ modelName one)
                                  , ("system.cpu.info.stepping", Any . Int $ stepping one)
                                  , ("system.cpu.info.microcode", Any . Text $ microcode one)
                                  , ("system.cpu.info.cpuMHz", Any . Double $ cpuMHz one)
                                  , ("system.cpu.info.cacheSize", Any . Text $ cacheSize one)
                                  , ("system.cpu.info.siblings", Any . Int $ siblings one)
                                  , ("system.cpu.info.apicid", Any . Int $ apicid one)
                                  , ("system.cpu.info.initial_apicid", Any . Int $ initialApicid one)
                                  , ("system.cpu.info.fpu", Any . Bool $ fpu one)
                                  , ("system.cpu.info.fpu_exception", Any . Bool $ fpuException one)
                                  , ("system.cpu.info.cpuid_level", Any . Int $ cpuidLevel one)
                                  , ("system.cpu.info.wp", Any . Bool $ wp one)
                                  , ("system.cpu.info.flags", AnyList $ map (Any . Text) (filter (/= "") $ flags one))
                                  , ("system.cpu.info.bogomips", Any . Double $ bogomips one)
                                  , ("system.cpu.info.cl_flush_size", Any . Int $ clflushSize one)
                                  , ("system.cpu.info.cache_alignment", Any . Int $ cacheAlignment one)
                                  , ("system.cpu.info.address_sizes", Any . Text $ addressSizes one)
                                  , ("system.cpu.info.power_management", Any . Text $ powerManagement one)
                                  ]
doCpuInfo _ = undefined

data CpuInf = CpuInf
  { processor :: Int
  , vendorId :: Text
  , cpuFamily :: Text
  , model :: Int
  , modelName :: Text
  , stepping :: Int
  , microcode :: Text
  , cpuMHz :: Double
  , cacheSize :: Text
  , physicalId :: Int
  , siblings :: Int
  , coreId :: Int
  , cpuCores :: Int
  , apicid :: Int
  , initialApicid :: Int
  , fpu :: Bool
  , fpuException :: Bool
  , cpuidLevel :: Int
  , wp :: Bool
  , flags :: [Text]
  , bogomips :: Double
  , clflushSize :: Int
  , cacheAlignment :: Int
  , addressSizes :: Text
  , powerManagement :: Text
  } deriving Show

parserYesNo :: Parser Bool
parserYesNo = (string "yes" *> pure True) <|> (string "no" *> pure False)

flag :: Parser Text
flag = takeWhile (notInClass " \n")

parserCpuInf :: Parser CpuInf
parserCpuInf = CpuInf <$> (string "processor" *> spaces *> char ':' *> spaces *> decimal  <* endOfLine )
                      <*> (string "vendor_id" *> spaces *> char ':' *> spaces *> takeTill isEndOfLine <* endOfLine )
                      <*> (string "cpu family" *> spaces *> char ':' *> spaces *> takeTill isEndOfLine <* endOfLine )
                      <*> (string "model" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "model name" *> spaces *> char ':' *> spaces *> takeTill isEndOfLine <* endOfLine )
                      <*> (string "stepping" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "microcode" *> spaces *> char ':' *> spaces *> takeTill isEndOfLine <* endOfLine )
                      <*> (string "cpu MHz" *> spaces *> char ':' *> spaces *> rational <* endOfLine )
                      <*> (string "cache size" *> spaces *> char ':' *> spaces *> takeTill isEndOfLine <* endOfLine )
                      <*> (string "physical id" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "siblings" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "core id" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "cpu cores" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "apicid" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "initial apicid" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "fpu" *> spaces *> char ':' *> spaces *> parserYesNo <* endOfLine )
                      <*> (string "fpu_exception" *> spaces *> char ':' *> spaces *> parserYesNo <* endOfLine )
                      <*> (string "cpuid level" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "wp" *> spaces *> char ':' *> spaces *> parserYesNo <* endOfLine )
                      <*> (string "flags" *> spaces *> char ':' *> (flag `sepBy` char ' ') <* endOfLine )
                      <*> (string "bogomips" *> spaces *> char ':' *> spaces *> rational <* endOfLine )
                      <*> (string "clflush size" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "cache_alignment" *> spaces *> char ':' *> spaces *> decimal <* endOfLine )
                      <*> (string "address sizes" *> spaces *> char ':' *> spaces *> takeTill isEndOfLine <* endOfLine )
                      <*> (string "power management" *> char ':' *> skipWhile isHorizontalSpace *> takeTill isEndOfLine <* endOfLine)


doCheck :: Check -> IO Complex
doCheck = undefined
{--
class Checkable a where
    route :: a -> (Name, Check -> IO Complex)
    describe :: a -> [(Field, Required, Description)]
    isCorrect :: Check -> a -> Either Text Check
    --}
