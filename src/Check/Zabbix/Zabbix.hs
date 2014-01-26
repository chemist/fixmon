{-# LANGUAGE OverloadedStrings #-}
module Check.Zabbix where

import Data.Conduit.Network
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary (sinkHandle, sourceLbs)
import Data.Conduit
import System.IO (stdout)
import Control.Concurrent
import Control.Monad.IO.Class (MonadIO)
import Data.Binary.Builder
import Data.Monoid
import Prelude hiding (length)


-- <HEADER> - "ZBXD\x01" (5 байт)
-- <DATALEN> - длина данных (8 байт). 1 будет сформировано как 01/00/00/00/00/00/00/00 (восемь байт в HEX, то есть число в 64-битном формате)
--


data LogType = All | Skip
data NetType = Bytes | Packets | Errors | Dropped
data Service = Ssh | Ntp | Ldap | Smtp | Ftp | Http | Pop | Nntp | Imap | Tcp | Https | Telnet
data ProcMemType = Avg | Max | Min | Sum
data ProcNumState = All' | Run | Sleep | Zomb
data SensorType = Avg' | Max' | Min'
data SystemCpuLoadCpu = All'' | Percpu
data SystemCpuLoadType = Avg1 | Avg5 | Avg15
data SystemCpuNumType = Online | Max''
data SystemCpuUtilType = Idle | Nice | User' | System | Iowait | Interrupt | Softirq | Steal
data SystemHostnameType = Netbios | Host
data SystemHwChassisType = Full'' | Model | Serial | Type | Vendor
data SystemHwCpuType = Full' | Curfreq | Maxfreq | Model' | Vendor'
data SystemHwDevicesType = Pci | Usb
data FullShort = Full | Short
data SystemLocaltimeType = Utc | Local
data SystemRunType = Wait | Nowait

data SystemStatSource = Ent
                      | Kthr KthrType
                      | Memory MemoryType
                      | Page PageType
                      | Faults FaultsType
                      | Cpu CpuType
                      | Disk DiskType
data KthrType = R | B
data MemoryType = Avm | Fre
data PageType = Fi | Fo | Pi | Po | Fr | Sr
data FaultsType = In | Sy | Cs
data CpuType = Us | Sy' | Id | Wa | Pc | Ec | Lbusy | App
data DiskType = Bps | Tps

data SystemSwOsType = Full''' | Short' | Name
data SystemSwPackagesManager = Dpkg | Pkgtool | Rpm | Pacman
data SystemSwapType = Count | Sectors | Pages
data SystemSwapSizeType = Free | Pfree | Pused | Total | Used
data VfsDevType = Sectors' | Operations | Bytes' | Sps | Ops | Bps'
data VfsFileTimeType = Modify | Access | Change
data VmMemorySizeType = Total' | Active | Anon | Buffers | Cached | Exec | File | Free' | Inactive | Pinned | Shared | Wired | Used' | Pused' | Available | Pavailable

type Regexp = L.ByteString
type Encoding = L.ByteString
type Ip = L.ByteString
type RecordType = L.ByteString
type Zone = L.ByteString
type Interface = L.ByteString
type Port = Int
type ProcessName = L.ByteString
type User = L.ByteString
type SensorName = L.ByteString
type Command = L.ByteString

data ZabbixKey = AgentHostname
               | AgentPing
               | AgentVersion
               | KernelMaxfiles
               | KernelMaxproc
               | ZLog FilePath (Maybe Regexp) (Maybe Encoding) Int LogType
               | Logrt FilePath (Maybe Regexp) (Maybe Encoding) Int LogType
               | NetDns (Maybe Ip) Zone (Maybe RecordType) (Maybe Int) (Maybe Int)
               | NetDnsRecord (Maybe Ip) Zone (Maybe RecordType) (Maybe Int) (Maybe Int)
               | NetIfCollisions Interface
               | NetIfIn Interface NetType
               | NetIfOut Interface NetType
               | NetIfTotal Interface NetType
               | NetTcpListen Port
               | NetTcpPort Ip Port
               | NetTcpService Service (Maybe Ip) (Maybe Port)
               | NetTcpServicePerf Service (Maybe Ip) (Maybe Port)
               | NetUdpListen Port
               | ProcMem (Maybe ProcessName) (Maybe User) (Maybe ProcMemType) (Maybe Regexp)
               | ProcNum (Maybe ProcessName) (Maybe User) (Maybe ProcNumState) (Maybe Regexp)
               | Sensor SensorName SensorName SensorType
               | SystemBoottime
               | SystemCpuIntr
               | SystemCpuLoad (Maybe SystemCpuLoadCpu) (Maybe SystemCpuLoadType)
               | SystemCpuNum (Maybe SystemCpuNumType)
               | SystemCpuSwitches
               | SystemCpuUtil (Maybe Int) (Maybe SystemCpuUtilType) (Maybe SystemCpuLoadType)
               | SystemHostname (Maybe SystemHostnameType)
               | SystemHwChassis (Maybe SystemHwChassisType)
               | SystemHwCpu (Maybe Int) (Maybe SystemHwCpuType)
               | SystemHwDevices (Maybe SystemHwDevicesType)
               | SystemHwMacaddr (Maybe Interface) (Maybe FullShort)
               | SystemLocaltime (Maybe SystemLocaltimeType)
               | SystemRun Command (Maybe SystemRunType)
               | SystemStat SystemStatSource
               | SystemSwArch
               | SystemSwOs SystemSwOsType
               | SystemSwPackages (Maybe Regexp) (Maybe SystemSwPackagesManager) (Maybe FullShort)
               | SystemSwapIn (Maybe L.ByteString) SystemSwapType
               | SystemSwapOut (Maybe L.ByteString) SystemSwapType
               | SystemSwapSize (Maybe L.ByteString) SystemSwapSizeType
               | SystemUname
               | SystemUptime
               | SystemUsersNum
               | VfsDevRead (Maybe L.ByteString) VfsDevType SystemCpuLoadType 
               | VfsDevWrite (Maybe L.ByteString) VfsDevType SystemCpuLoadType
               | VfsFileCksum FilePath
               | VfsFileExists FilePath
               | VfsFileMd5sum FilePath
               | VfsFileRegexp FilePath Regexp (Maybe Encoding)
               | VfsFileRegmatch FilePath Regexp (Maybe Encoding)
               | VfsFileSize FilePath
               | VfsFileTime FilePath (Maybe VfsFileTimeType)
               | VfsFsInode L.ByteString (Maybe SystemSwapSizeType)
               | VfsFsSize  L.ByteString (Maybe SystemSwapSizeType)
               | VmMemorySize (Maybe VmMemorySizeType)
               | WebPageGet L.ByteString (Maybe L.ByteString) (Maybe Port)
               | WebPagePerf L.ByteString (Maybe L.ByteString) (Maybe Port)
               | WebPageRegexp L.ByteString (Maybe L.ByteString) (Maybe Port) Regexp Int

header :: L.ByteString
header = "ZBXD\x01"

makeLength :: L.ByteString -> L.ByteString
makeLength x = let w = toEnum . fromEnum . L.length $ x
                   b = putWord64le w
               in toLazyByteString b

agentHost :: ByteString
agentHost = "limbo"

agentPort :: Int
agentPort = 10050

main :: IO ()
main = runTCPClient (clientSettings agentPort agentHost) $ \app -> do
    forkIO $ getter $$ appSink app
    appSource app $$ sinkHandle stdout


getter :: MonadIO m => Source m ByteString
getter = let d = "system.cpu.util[]\n" 
             l = makeLength d 
         in sourceLbs $ "ZBXD\x01" <> l <> d

