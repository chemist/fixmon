{-# LANGUAGE OverloadedStrings #-}
module Check.Vfs where

import Types

import           Data.Map              (fromList, singleton)

data Vfs = Discovery
         | Inode
         | Size
         | FileTime
         | FileSize
         | FileRegMatch
         | FileRegexp
         | FileMD5
         | FileExists
         | FileContents
         | DevStat
         deriving (Show)

instance Checkable Vfs where
    route Discovery             = singleton "vfs.fs.discovery" doDiscovery
    route Inode                 = singleton "vfs.fs.inode"     doInode
    route Size                  = singleton "vfs.fs.size"     doInode
    route DevStat               = singleton "vfs.fs.stats"     doInode
    route FileTime              = singleton "vfs.file.time"     doInode
    route FileSize              = singleton "vfs.file.size"     doInode
    route FileRegMatch          = singleton "vfs.file.regmatch"     doInode
    route FileRegexp            = singleton "vfs.file.regexp"     doInode
    route FileMD5               = singleton "vfs.file.md5"     doInode
    route FileExists            = singleton "vfs.file.exists"     doInode
    route FileContents          = singleton "vfs.file.contents"     doInode

    routeCheck Discovery    = routeCheck'  Discovery    "vfs.fs.discovery"
    routeCheck Inode        = routeCheck'  Inode        "vfs.fs.inode"
    routeCheck Size         = routeCheck'  Size         "vfs.fs.size"
    routeCheck DevStat      = routeCheck'  DevStat      "vfs.fs.stats"
    routeCheck FileTime     = routeCheck'  FileTime     "vfs.file.time"
    routeCheck FileSize     = routeCheck'  FileSize     "vfs.file.size"
    routeCheck FileRegMatch = routeCheck'  FileRegMatch "vfs.file.regmatch"
    routeCheck FileRegexp   = routeCheck'  FileRegexp   "vfs.file.regexp"
    routeCheck FileMD5      = routeCheck'  FileMD5      "vfs.file.md5"
    routeCheck FileExists   = routeCheck'  FileExists   "vfs.file.exists"
    routeCheck FileContents = routeCheck'  FileContents "vfs.file.contents"

    describe Discovery    = []
    describe Inode        = []
    describe Size         = []
    describe DevStat      = []
    describe FileTime     = []
    describe FileSize     = []
    describe FileRegMatch = []
    describe FileRegexp   = []
    describe FileMD5      = []
    describe FileExists   = []

doDiscovery = undefined
doInode = undefined
