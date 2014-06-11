{-# LANGUAGE OverloadedStrings #-}
module Check.System where

import           Check
import           Types

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Map              (fromList)
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Text             (Text, isPrefixOf, pack, toLower)
import           Data.Text.IO          (readFile)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           System.Cron           (daily)

import           Network.BSD           (getHostName)
import           Prelude               hiding (readFile, takeWhile)

-- see http://man7.org/linux/man-pages/man5/proc.5.html

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
    route CpuSwitches  =  ("system.cpu.switches",   doCpuSwitches)
    route CpuUtil  =  ("system.cpu.util",   doCpuUtil)
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
statFile :: String
statFile = "/proc/stat"

-- | check, read /proc/stat, return boottime as UTCTime
doBootTime :: Check -> IO Complex
doBootTime (Check _ _ "system.boottime" _) = do
    Right t <- parseOnly parserBootTime <$> readFile statFile
    let bootTime = posixSecondsToUTCTime t
    return $ Complex $ fromList [ ("system.boottime", Any . UTC $ bootTime )]
doBootTime _ = undefined

-- | test check for doBootTime
testBootTime = Check (CheckName "boottime") (Cron daily) "system.boottime" (fromList [])

-- | helpers for doBootTime
parserBootTime :: Parser NominalDiffTime
parserBootTime = head . catMaybes <$> bootOrEmpty `sepBy` endOfLine
  where bootOrEmpty = Just <$> (string "btime" *> space *> rational) <|> pure Nothing <* takeTill isEndOfLine
--------------------------------------------------------------------------------------

testIntr :: Check
testIntr = Check (CheckName "interrupts") (Cron daily) "system.cpu.intr" (fromList [])

intrFile :: String
intrFile = "/proc/interrupts"
-- intrFile = "interrupts"

doCpuIntr :: Check -> IO Complex
doCpuIntr (Check _ _ "system.cpu.intr" _) = do
    Right c <- parseOnly parserInterrupts <$> readFile intrFile
    return $ Complex $ fromList $ mkInterrupts c
doCpuIntr _ = undefined

mkInterrupts :: (Cpu, [Interrupt]) -> [(Tag, Any)]
mkInterrupts (Cpu c, i) =
  let lowerCpuN = map toLower c
      addAll xs = ("allcpu", sum $ map snd xs) : xs
      mk (Interrupt t ns _) =  map (\(x, y) -> ("system.cpu.intr." <> x <> "." <> toLower t, toAny y)) $ addAll $ zip lowerCpuN ns
      intr = concatMap mk i
  in interrupsAll intr : intr

interrupsAll :: [(Tag, Any)] -> (Tag, Any)
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
    return . Complex . fromList $ [ ("system.cpu.loadavg.la1", Any . Double $ x)
                                  , ("system.cpu.loadavg.la5", Any . Double $ y)
                                  , ("system.cpu.loadavg.la15", Any . Double $ z)
                                  ]
doCpuLoad _ = undefined

parserLoadavg :: Parser (Double, Double, Double)
parserLoadavg = (,,) <$> rational <* space <*> rational <* space <*> rational
--------------------------------------------------------------------------------------

cpuFile :: String
-- cpuFile = "cpuinfo"
cpuFile = "/proc/cpuinfo"

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
  { processor       :: Int
  , vendorId        :: Text
  , cpuFamily       :: Text
  , model           :: Int
  , modelName       :: Text
  , stepping        :: Int
  , microcode       :: Text
  , cpuMHz          :: Double
  , cacheSize       :: Text
  , physicalId      :: Int
  , siblings        :: Int
  , coreId          :: Int
  , cpuCores        :: Int
  , apicid          :: Int
  , initialApicid   :: Int
  , fpu             :: Bool
  , fpuException    :: Bool
  , cpuidLevel      :: Int
  , wp              :: Bool
  , flags           :: [Text]
  , bogomips        :: Double
  , clflushSize     :: Int
  , cacheAlignment  :: Int
  , addressSizes    :: Text
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
--------------------------------------------------------------------------------------

testCpuSwitches :: Check
testCpuSwitches = Check (CheckName "switches") (Cron daily) "system.cpu.switches" (fromList [])

doCpuSwitches :: Check -> IO Complex
doCpuSwitches (Check _ _ "system.cpu.switches" _) = do
    Right s <- parseOnly parserCpuSwitches <$> readFile statFile
    return . Complex . fromList $ [ ("system.cpu.switches", Any . Int $ s)]

parserCpuSwitches :: Parser Int
parserCpuSwitches = head . catMaybes <$> switchesOrEmpty `sepBy` endOfLine
  where switchesOrEmpty = Just <$> (string "ctxt" *> space *> decimal) <|> pure Nothing <* takeTill isEndOfLine
--------------------------------------------------------------------------------------

testCpuUtil :: Check
testCpuUtil = Check (CheckName "cpuutil") (Cron daily) "system.cpu.util" (fromList [])

doCpuUtil :: Check -> IO Complex
doCpuUtil (Check _ _ "system.cpu.util" _) = do
  Right c <- parseOnly parserProcStatCpu <$> readFile statFile
  let ifJust (name, Nothing) = Nothing
      ifJust (name, (Just i)) = Just (name, Any . Int $ i)
      ifJustAll = map ifJust [ ("system.cpu.util.iowait", iowait c)
                             , ("system.cpu.util.irq", irq c)
                             , ("system.cpu.util.softirq", softirq c)
                             , ("system.cpu.util.steal", steal c)
                             , ("system.cpu.util.guest", guest c)
                             , ("system.cpu.util.guestnice", guestNice c)
                             ]
  return . Complex . fromList $ [ ( "system.cpu.util.user", Any . Int $ user c)
                                , ( "system.cpu.util.nice", Any . Int $ nice c)
                                , ( "system.cpu.util.system", Any . Int $ system c)
                                , ( "system.cpu.util.idle"  , Any . Int $ idle c)
                                ] ++ catMaybes ifJustAll

parserProcStatCpu :: Parser CpuUtilStat
parserProcStatCpu = head . catMaybes <$> (cpuOrEmpty `sepBy` endOfLine)
  where
    cpuOrEmpty = Just <$> cpuUtilStat <|> pure Nothing <* takeTill isEndOfLine
    cpuUtilStat = do
        string "cpu "
        xs <- many (space *> decimal)
        return $ CpuUtilStat
          { user = xs !! 0
          , nice = xs !! 1
          , system = xs !! 2
          , idle = xs !! 3
          , iowait = xs !!? 4
          , irq = xs !!? 5
          , softirq = xs !!? 6
          , steal = xs !!? 7
          , guest = xs !!? 8
          , guestNice = xs !!? 9
          }

-- | Safe index
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i = safeIndex i (length xs) xs
  where safeIndex i is xs | i < is = Just (xs !! i)
                          | otherwise = Nothing

data CpuUtilStat = CpuUtilStat
  { user      :: Int
  , nice      :: Int
  , system    :: Int
  , idle      :: Int
  , iowait    :: Maybe Int
  , irq       :: Maybe Int
  , softirq   :: Maybe Int
  , steal     :: Maybe Int
  , guest     :: Maybe Int
  , guestNice :: Maybe Int
  } deriving Show

--------------------------------------------------------------------------------------

doCheck :: Check -> IO Complex
doCheck = undefined
{--
class Checkable a where
    route :: a -> (Name, Check -> IO Complex)
    describe :: a -> [(Field, Required, Description)]
    isCorrect :: Check -> a -> Either Text Check
    --}
