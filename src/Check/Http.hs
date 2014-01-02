{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Check.Http where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.Attoparsec.Text     (parseOnly)
import           Data.Map                 (Map, fromList)
import           Data.Text
import           GNS.Data
import           System.Cron.Parser

testCheck :: Check
testCheck = Check
  { _checkName = "ya"
  , _period = Cron . right $ parseOnly cronSchedule  "* * * * *"
  , _params = fromList []
  }

right ::Either String b -> b
right (Right x) = x
right _ = error "bad crontab"
