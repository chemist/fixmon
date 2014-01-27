{-# LANGUAGE OverloadedStrings #-}
module Check.Zabbix.Types where

import Data.ByteString.Lazy hiding (length, pack)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as L
import Data.Binary.Builder (putWord64le, toLazyByteString)
import Data.Monoid ((<>))
import Prelude hiding (length)

header :: ByteString
header = "ZBXD\x01"

length :: ByteString -> ByteString
length x = let w = toEnum . fromEnum . L.length $ x
               b = putWord64le w
           in toLazyByteString b

class Zabbix a where
    raw :: a -> ByteString

instance Zabbix Int where
    raw x = pack . show $ x 

instance Zabbix ByteString where
    raw = id

instance Zabbix a => Zabbix (Maybe a) where
    raw (Just a) = raw a
    raw Nothing = ""

data LogType = All | Skip
instance Zabbix LogType where
    raw All = "all"
    raw Skip = "skip"

data NetType = Bytes | Packets | Errors | Dropped
instance Zabbix NetType where
    raw Bytes = "bytes"
    raw Packets = "packets"
    raw Errors = "errors"
    raw Dropped = "dropped"

data Service = Ssh | Ntp | Ldap | Smtp | Ftp | Http | Pop | Nntp | Imap | Tcp | Https | Telnet
instance Zabbix Service where
    raw Ssh = "ssh"
    raw Ntp = "ntp"
    raw Ldap = "ldap"
    raw Smtp = "smtp"
    raw Ftp = "ftp"
    raw Http = "http"
    raw Pop = "pop"
    raw Nntp = "nntp"
    raw Imap = "imap"
    raw Tcp = "tcp"
    raw Https = "https"
    raw Telnet = "telnet"

data ProcMemType = Avg | Max | Min | Sum
instance Zabbix ProcMemType where
    raw Avg = "avg"
    raw Max = "max"
    raw Min = "min"
    raw Sum = "sum"

data ProcNumState = All' | Run | Sleep | Zomb
instance Zabbix ProcNumState where
    raw All' = "all"
    raw Run = "run"
    raw Sleep = "sleep"
    raw Zomb = "zomb"

data SensorType = Avg' | Max' | Min'
instance Zabbix SensorType where
    raw Avg' = "avg"
    raw Max' = "max"
    raw Min' = "min"

data SystemCpuLoadCpu = All'' | Percpu
instance Zabbix SystemCpuLoadCpu where
    raw All'' = "all"
    raw Percpu = "percpu"

data SystemCpuLoadType = Avg1 | Avg5 | Avg15
instance Zabbix SystemCpuLoadType where
    raw Avg1 = "avg1"
    raw Avg5 = "avg5"
    raw Avg15 = "avg15"

data SystemCpuNumType = Online | Max''
instance Zabbix SystemCpuNumType where
    raw Online = "online"
    raw Max'' = "max"

data SystemCpuUtilType = Idle | Nice | User' | System | Iowait | Interrupt | Softirq | Steal
instance Zabbix SystemCpuUtilType where
    raw Idle = "idle"
    raw Nice = "nice"
    raw User' = "user"
    raw System = "system"
    raw Iowait = "iowait"
    raw Interrupt = "interrupt"
    raw Softirq = "softirq"
    raw Steal = "steal"

data SystemHostnameType = Netbios | Host
instance Zabbix SystemHostnameType where
    raw Netbios = "netbios"
    raw Host = "host"
    
data SystemHwChassisType = Full'' | Model | Serial | Type | Vendor
instance Zabbix SystemHwChassisType where
    raw Full'' = "full"
    raw Model = "model"
    raw Serial = "serial"
    raw Type = "type"
    raw Vendor = "vendor"

data SystemHwCpuType = Full' | Curfreq | Maxfreq | Model' | Vendor'
instance Zabbix SystemHwCpuType where
    raw Full' = "full"
    raw Curfreq = "curfreq"
    raw Maxfreq = "maxfreq"
    raw Model' = "model"
    raw Vendor' = "vendor"

data SystemHwDevicesType = Pci | Usb
instance Zabbix SystemHwDevicesType where
    raw Pci = "pci"
    raw Usb = "usb"

data FullShort = Full | Short
instance Zabbix FullShort where
    raw Full = "full"
    raw Short = "short"

data SystemLocaltimeType = Utc | Local
instance Zabbix SystemLocaltimeType where
    raw Utc = "utc"
    raw Local = "local"

data SystemRunType = Wait | Nowait
instance Zabbix SystemRunType where
    raw Wait = "wait"
    raw Nowait = "nowait"

data SystemStatSource = Ent
                      | Kthr KthrType
                      | Memory MemoryType
                      | Page PageType
                      | Faults FaultsType
                      | Cpu CpuType
                      | Disk DiskType
instance Zabbix SystemStatSource where
    raw Ent = "ent"
    raw (Kthr x) = "kthr," <> raw x
    raw (Memory x) = "memory," <> raw x
    raw (Page x) = "page," <> raw x
    raw (Faults x) = "faults," <> raw x
    raw (Cpu x) = "cpu," <> raw x
    raw (Disk x) = "disk," <> raw x

data KthrType = R | B
instance Zabbix KthrType where
    raw R = "r"
    raw B = "b"

data MemoryType = Avm | Fre
instance Zabbix MemoryType where
    raw Avm = "avm"
    raw Fre = "fre"

data PageType = Fi | Fo | Pi | Po | Fr | Sr
instance Zabbix PageType where
    raw Fi = "fi"
    raw Fo = "fo"
    raw Pi = "pi"
    raw Po = "po"
    raw Fr = "fr"
    raw Sr = "sr"

data FaultsType = In | Sy | Cs
instance Zabbix FaultsType where
    raw In = "in"
    raw Sy = "sy"
    raw Cs = "cs"

data CpuType = Us | Sy' | Id | Wa | Pc | Ec | Lbusy | App
instance Zabbix CpuType where
    raw Us = "us"
    raw Sy' = "sy"
    raw Id = "id"
    raw Wa = "wa"
    raw Pc = "pc"
    raw Ec = "ec"
    raw Lbusy = "lbusy"
    raw App = "app"

data DiskType = Bps | Tps
instance Zabbix DiskType where
    raw Bps = "bps"
    raw Tps = "tps"


data SystemSwOsType = Full''' | Short' | Name
instance Zabbix SystemSwOsType where
    raw Full''' = "full"
    raw Short' = "short"
    raw Name = "name"

data SystemSwPackagesManager = Dpkg | Pkgtool | Rpm | Pacman
instance Zabbix SystemSwPackagesManager where
    raw Dpkg = "dpkg"
    raw Pkgtool = "pkgtool"
    raw Rpm = "rpm"
    raw Pacman = "packman"

data SystemSwapType = Count | Sectors | Pages
instance Zabbix SystemSwapType where
    raw Count = "count"
    raw Sectors = "sectors"
    raw Pages = "pages"

data SystemSwapSizeType = Free | Pfree | Pused | Total | Used
instance Zabbix SystemSwapSizeType where
    raw Free = "free"
    raw Pfree = "pfree"
    raw Pused = "pused"
    raw Total = "total"
    raw Used = "used"
    
data VfsDevType = Sectors' | Operations | Bytes' | Sps | Ops | Bps'
instance Zabbix VfsDevType where
    raw Sectors' = "sectors"
    raw Operations = "operations"
    raw Bytes' = "bytes"
    raw Sps = "sps"
    raw Ops = "ops"
    raw Bps' = "bps"

data VfsFileTimeType = Modify | Access | Change
instance Zabbix VfsFileTimeType where
    raw Modify = "modify"
    raw Access = "access"
    raw Change = "change"

data VmMemorySizeType = Total' | Active | Anon | Buffers | Cached | Exec | File | Free' | Inactive | Pinned | Shared | Wired | Used' | Pused' | Available | Pavailable
instance Zabbix VmMemorySizeType where
    raw Total' = "total"
    raw Active = "active"
    raw Anon = "anon"
    raw Buffers = "buffers"
    raw Cached = "cached"
    raw Exec = "exec"
    raw File = "file"
    raw Free' = "free"
    raw Inactive = "inactive"
    raw Pinned = "pinned"
    raw Shared = "shared"
    raw Wired = "wired"
    raw Used' = "used"
    raw Pused' = "pused"
    raw Available = "available"
    raw Pavailable = "pavailable"

type Regexp = ByteString
type Encoding = ByteString
type Ip = ByteString
type RecordType = ByteString
type Zone = ByteString
type Interface = ByteString
type Port = Int
type ProcessName = ByteString
type User = ByteString
type SensorName = ByteString
type Command = ByteString
type FPath = ByteString

data ZabbixKey = AgentHostname
               | AgentPing
               | AgentVersion
               | KernelMaxfiles
               | KernelMaxproc
               | ZLog FPath (Maybe Regexp) (Maybe Encoding) Int (Maybe LogType)
               | Logrt FPath (Maybe Regexp) (Maybe Encoding) Int LogType
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
               | SystemSwapIn (Maybe ByteString) SystemSwapType
               | SystemSwapOut (Maybe ByteString) SystemSwapType
               | SystemSwapSize (Maybe ByteString) SystemSwapSizeType
               | SystemUname
               | SystemUptime
               | SystemUsersNum
               | VfsDevRead (Maybe ByteString) VfsDevType SystemCpuLoadType 
               | VfsDevWrite (Maybe ByteString) VfsDevType SystemCpuLoadType
               | VfsFileCksum FPath
               | VfsFileExists FPath
               | VfsFileMd5sum FPath
               | VfsFileRegexp FPath Regexp (Maybe Encoding)
               | VfsFileRegmatch FPath Regexp (Maybe Encoding)
               | VfsFileSize FPath
               | VfsFileTime FPath (Maybe VfsFileTimeType)
               | VfsFsInode ByteString (Maybe SystemSwapSizeType)
               | VfsFsSize  ByteString (Maybe SystemSwapSizeType)
               | VmMemorySize (Maybe VmMemorySizeType)
               | WebPageGet ByteString (Maybe ByteString) (Maybe Port)
               | WebPagePerf ByteString (Maybe ByteString) (Maybe Port)
               | WebPageRegexp ByteString (Maybe ByteString) (Maybe Port) Regexp Int

zabbix :: ZabbixKey -> ByteString
zabbix x = header <> rw x <> raw x
  where 
  rw = length . raw


instance Zabbix ZabbixKey where
    raw AgentHostname = "agent.hostname\n"
    raw AgentPing = "agent.ping\n"
    raw AgentVersion = "agent.version\n"
    raw KernelMaxfiles = "kernel.maxfiles\n"
    raw KernelMaxproc = "kernel.maxproc\n"
    raw (ZLog path mr me i lt) = "log[" <> path <> "," <> raw mr <> "," <> raw me <> "," <> raw i <> "," <> raw lt <> "]\n"


