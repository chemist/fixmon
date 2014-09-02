{-# LANGUAGE OverloadedStrings #-}
module Check.System where

import           Types

import           Control.Applicative
import           Control.Monad         (void)
import           Data.Attoparsec.Text
import           Data.Map.Strict       (singleton)
import           Data.Maybe
import           Data.Monoid           ((<>))
import           Data.Text             (Text, isPrefixOf, pack, toLower)
import           Data.Text.IO          (readFile)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime   (getCurrentTimeZone, timeZoneName)
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
    route HostName =     singleton "system.hostname"       doHostname
    route Uptime   =     singleton "system.uptime"         doUptime
    route Boottime =     singleton "system.boottime"       doBootTime
    route CpuIntr  =     singleton "system.cpu.intr"       doCpuIntr
    route CpuLoad  =     singleton "system.cpu.loadavg"    doCpuLoad
    route CpuInfo  =     singleton "system.cpu.info"       doCpuInfo
    route CpuSwitches  = singleton "system.cpu.switches"   doCpuSwitches
    route CpuUtil  =     singleton "system.cpu.util"       doCpuUtil
    route LocalTime  =   singleton "system.localtime"      doLocalTime

    routeCheck HostName    = routeCheck'  HostName    "system.hostname"
    routeCheck Uptime      = routeCheck'  Uptime      "system.uptime"
    routeCheck Boottime    = routeCheck'  Boottime    "system.boottime"
    routeCheck CpuIntr     = routeCheck'  CpuIntr     "system.cpu.intr"
    routeCheck CpuLoad     = routeCheck'  CpuLoad     "system.cpu.loadavg"
    routeCheck CpuInfo     = routeCheck'  CpuInfo     "system.cpu.info"
    routeCheck CpuSwitches = routeCheck'  CpuSwitches "system.cpu.switches"
    routeCheck CpuUtil     = routeCheck'  CpuUtil     "system.cpu.util"
    routeCheck LocalTime   = routeCheck'  LocalTime   "system.localtime"

    describe HostName = []
    describe Uptime   = []
    describe Boottime   = []
    describe LocalTime   = []
    describe CpuIntr = []
    describe CpuLoad = []
    describe CpuInfo = []
    describe CpuSwitches = []
    describe CpuUtil = []

---------------------------------- linux checks --------------------------------------
doHostname :: Check -> IO Complex
doHostname (Check _ _ "system.hostname" _) = do
    h <- getHostName
    return $ Complex [ ( "system.hostname", toDyn (pack h)) ]
doHostname _ = error "system check"

testHostname :: Check
testHostname = Check (CheckName "hostname") (Cron daily) "system.hostname" []
--------------------------------------------------------------------------------------

-- "3023604.41 11190196.16\n"
uptimeFile :: String
uptimeFile = "/proc/uptime"

doUptime :: Check -> IO Complex
doUptime (Check _ _ "system.uptime" _) = do
    Done _ (up, idle') <-  parse parserUptime <$> readFile uptimeFile
    return $ Complex [ ("system.uptime.up", toDyn (timeToPeriod up))
                     , ("system.uptime.idle", toDyn (timeToPeriod idle'))
                     ]
    where
       timeToPeriod x =
         let i = truncate x
             days = i `div` 86400
             hours = (i `mod` 86400) `div` 3600
             mins = (i `mod` 3600) `div` 60
             showD = if days /= (0 :: Integer) then pack (show days) <> " days " else ""
             showH = if hours /= 0 then pack (show hours) <> " hours " else ""
             showM = if mins /= 0 then pack (show mins) <> " minunes" else ""
         in "up " <> showD <> showH <> showM
doUptime _ = error "system check"

parserUptime :: Parser (Double, Double)
parserUptime = (,) <$> rational <* space <*> rational <* endOfLine

testUptime :: Check
testUptime = Check (CheckName "uptime") (Cron daily) "system.uptime" []
--------------------------------------------------------------------------------------
statFile :: String
statFile = "/proc/stat"

-- | check, read /proc/stat, return boottime as UTCTime
doBootTime :: Check -> IO Complex
doBootTime (Check _ _ "system.boottime" _) = do
    Right t <- parseOnly parserBootTime <$> readFile statFile
    let bootTime = posixSecondsToUTCTime t
    return $ Complex [ ("system.boottime", toDyn bootTime )]
doBootTime _ = error "system check"

-- | test check for doBootTime
testBootTime :: Check
testBootTime = Check (CheckName "boottime") (Cron daily) "system.boottime" []

-- | helpers for doBootTime
parserBootTime :: Parser NominalDiffTime
parserBootTime = head . catMaybes <$> bootOrEmpty `sepBy` endOfLine
  where bootOrEmpty = Just <$> (string "btime" *> space *> rational) <|> pure Nothing <* takeTill isEndOfLine
--------------------------------------------------------------------------------------

testIntr :: Check
testIntr = Check (CheckName "interrupts") (Cron daily) "system.cpu.intr" []

intrFile :: String
intrFile = "/proc/interrupts"
-- intrFile = "interrupts"

doCpuIntr :: Check -> IO Complex
doCpuIntr (Check _ _ "system.cpu.intr" _) = do
    Right c <- parseOnly parserInterrupts <$> readFile intrFile
    return $ Complex $ mkInterrupts c
doCpuIntr _ = error "system check"

mkInterrupts :: (Cpu, [Interrupt]) -> [(Counter, Dyn)]
mkInterrupts (Cpu c, i) =
  let lowerCpuN = map toLower c
      addAll xs = map (\(x, y) -> (Counter x, y)) $ ("allcpu", sum $ map snd xs) : xs
      mk (Interrupt t ns _) =  map (\(Counter x, y) -> (Counter ("system.cpu.intr." <> x <> "." <> toLower t), toDyn y)) $ addAll $ zip lowerCpuN ns
      intr = concatMap mk i
  in interrupsAll intr : intr

interrupsAll :: [(Counter, Dyn)] -> (Counter, Dyn)
interrupsAll xs =
  let onlyAll = filter (\(Counter x, _) -> isPrefixOf "system.cpu.intr.allcpu" x) xs
  in ("system.cpu.intr.total", toDyn $ (sum $ map (\(_, x) -> fromDyn x)  onlyAll :: Int))

spaces :: Parser ()
spaces = skipWhile isHorizontalSpace

cpuN :: Parser Text
cpuN = takeWhile1 (inClass "a-zA-Z0-9")

data Cpu = Cpu {-# UNPACK #-} ![Text] deriving (Show, Eq)
data Interrupt = Interrupt {-# UNPACK #-} !Text {-# UNPACK #-} ![Int] {-# UNPACK #-} !Text deriving (Show, Eq)

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
testLoadAvg = Check (CheckName "loadavg") (Cron daily) "system.cpu.loadavg" []

doCpuLoad :: Check -> IO Complex
doCpuLoad (Check _ _ "system.cpu.loadavg" _) = do
    Right (x,y,z) <- parseOnly parserLoadavg <$> readFile loadavgFile
    return $ Complex [ ("system.cpu.loadavg.la1", toDyn x)
                     , ("system.cpu.loadavg.la5", toDyn y)
                     , ("system.cpu.loadavg.la15", toDyn z)
                     ]
doCpuLoad _ = error "system check"

parserLoadavg :: Parser (Double, Double, Double)
parserLoadavg = (,,) <$> rational <* space <*> rational <* space <*> rational
--------------------------------------------------------------------------------------

cpuFile :: String
-- cpuFile = "cpuinfo"
cpuFile = "/proc/cpuinfo"

testCpuInfo :: Check
testCpuInfo = Check (CheckName "cpuinfo") (Cron daily) "system.cpu.info" []

doCpuInfo :: Check -> IO Complex
doCpuInfo (Check _ _ "system.cpu.info" _) = do
    Right cpus <- parseOnly (parserCpuInf `sepBy` char '\n') <$> readFile cpuFile
    let one = head cpus
    return $ Complex [ ("system.cpu.info.num", toDyn $ length cpus)
                     , ("system.cpu.info.vendor_id", toDyn $ vendorId  one)
                     , ("system.cpu.info.cpu_family", toDyn $ cpuFamily one)
                     , ("system.cpu.info.model", toDyn $ model one)
                     , ("system.cpu.info.model_name", toDyn $ modelName one)
                     , ("system.cpu.info.stepping", toDyn $ stepping one)
                     , ("system.cpu.info.microcode", toDyn $ microcode one)
                     , ("system.cpu.info.cpuMHz", toDyn $ cpuMHz one)
                     , ("system.cpu.info.cacheSize", toDyn $ cacheSize one)
                     , ("system.cpu.info.siblings", toDyn $ siblings one)
                     , ("system.cpu.info.apicid", toDyn $ apicid one)
                     , ("system.cpu.info.initial_apicid", toDyn $ initialApicid one)
                     , ("system.cpu.info.fpu", toDyn $ fpu one)
                     , ("system.cpu.info.fpu_exception", toDyn $ fpuException one)
                     , ("system.cpu.info.cpuid_level", toDyn $ cpuidLevel one)
                     , ("system.cpu.info.wp", toDyn $ wp one)
                     , ("system.cpu.info.flags", DynList $ map toDyn (filter (/= "") $ flags one))
                     , ("system.cpu.info.bogomips", toDyn $ bogomips one)
                     , ("system.cpu.info.cl_flush_size", toDyn $ clflushSize one)
                     , ("system.cpu.info.cache_alignment", toDyn $ cacheAlignment one)
                     , ("system.cpu.info.address_sizes", toDyn $ addressSizes one)
                     , ("system.cpu.info.power_management", toDyn $ powerManagement one)
                     ]
doCpuInfo _ = error "system check"

data CpuInf = CpuInf
  { processor       :: {-# UNPACK #-} !Int
  , vendorId        :: {-# UNPACK #-} !Text
  , cpuFamily       :: {-# UNPACK #-} !Text
  , model           :: {-# UNPACK #-} !Int
  , modelName       :: {-# UNPACK #-} !Text
  , stepping        :: {-# UNPACK #-} !Int
  , microcode       :: {-# UNPACK #-} !Text
  , cpuMHz          :: {-# UNPACK #-} !Double
  , cacheSize       :: {-# UNPACK #-} !Text
  , physicalId      :: {-# UNPACK #-} !Int
  , siblings        :: {-# UNPACK #-} !Int
  , coreId          :: {-# UNPACK #-} !Int
  , cpuCores        :: {-# UNPACK #-} !Int
  , apicid          :: {-# UNPACK #-} !Int
  , initialApicid   :: {-# UNPACK #-} !Int
  , fpu             :: {-# UNPACK #-} !Bool
  , fpuException    :: {-# UNPACK #-} !Bool
  , cpuidLevel      :: {-# UNPACK #-} !Int
  , wp              :: {-# UNPACK #-} !Bool
  , flags           :: {-# UNPACK #-} ![Text]
  , bogomips        :: {-# UNPACK #-} !Double
  , clflushSize     :: {-# UNPACK #-} !Int
  , cacheAlignment  :: {-# UNPACK #-} !Int
  , addressSizes    :: {-# UNPACK #-} !Text
  , powerManagement :: {-# UNPACK #-} !Text
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
testCpuSwitches = Check (CheckName "switches") (Cron daily) "system.cpu.switches" []

doCpuSwitches :: Check -> IO Complex
doCpuSwitches (Check _ _ "system.cpu.switches" _) = do
    Right s <- parseOnly parserCpuSwitches <$> readFile statFile
    return . Complex $ [ ("system.cpu.switches", toDyn s)]
doCpuSwitches _ = error "system check"

parserCpuSwitches :: Parser Int
parserCpuSwitches = head . catMaybes <$> switchesOrEmpty `sepBy` endOfLine
  where switchesOrEmpty = Just <$> (string "ctxt" *> space *> decimal) <|> pure Nothing <* takeTill isEndOfLine
--------------------------------------------------------------------------------------

testCpuUtil :: Check
testCpuUtil = Check (CheckName "cpuutil") (Cron daily) "system.cpu.util" []

doCpuUtil :: Check -> IO Complex
doCpuUtil (Check _ _ "system.cpu.util" _) = do
  Right c <- parseOnly parserProcStatCpu <$> readFile statFile
  let ifJust (_, Nothing) = Nothing
      ifJust (name, Just i) = Just (name, toDyn i)
      ifJustAll = map ifJust [ ("system.cpu.util.iowait", iowait c)
                             , ("system.cpu.util.irq", irq c)
                             , ("system.cpu.util.softirq", softirq c)
                             , ("system.cpu.util.steal", steal c)
                             , ("system.cpu.util.guest", guest c)
                             , ("system.cpu.util.guestnice", guestNice c)
                             ]
  return . Complex $ [ ( "system.cpu.util.user", toDyn $ user c)
                     , ( "system.cpu.util.nice", toDyn $ nice c)
                     , ( "system.cpu.util.system", toDyn $ system c)
                     , ( "system.cpu.util.idle"  , toDyn $ idle c)
                     ] ++ catMaybes ifJustAll
doCpuUtil _ = error "system check"

parserProcStatCpu :: Parser CpuUtilStat
parserProcStatCpu = head . catMaybes <$> (cpuOrEmpty `sepBy` endOfLine)
  where
    cpuOrEmpty = Just <$> cpuUtilStat <|> pure Nothing <* takeTill isEndOfLine
    cpuUtilStat = do
        void $ string "cpu "
        xs <- many (space *> decimal)
        return CpuUtilStat
          { user = head xs
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
  where safeIndex i' is xs' | i' < is = Just (xs' !! i')
                            | otherwise = Nothing

data CpuUtilStat = CpuUtilStat
  { user      :: {-# UNPACK #-} !Int
  , nice      :: {-# UNPACK #-} !Int
  , system    :: {-# UNPACK #-} !Int
  , idle      :: {-# UNPACK #-} !Int
  , iowait    :: {-# UNPACK #-} !(Maybe Int)
  , irq       :: {-# UNPACK #-} !(Maybe Int)
  , softirq   :: {-# UNPACK #-} !(Maybe Int)
  , steal     :: {-# UNPACK #-} !(Maybe Int)
  , guest     :: {-# UNPACK #-} !(Maybe Int)
  , guestNice :: {-# UNPACK #-} !(Maybe Int)
  } deriving Show

--------------------------------------------------------------------------------------

doLocalTime :: Check -> IO Complex
doLocalTime (Check _ _ "system.localtime" _) = do
    t <- getCurrentTime
    z <- timeZoneName <$> getCurrentTimeZone
    return . Complex $ [ ("system.localtime.utc", toDyn t)
                       , ("system.localtime.zone", toDyn . pack $ z)
                       ]
doLocalTime _ = error "system check"
