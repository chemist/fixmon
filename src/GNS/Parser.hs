{-# LANGUAGE OverloadedStrings #-}
module GNS.Parser ( parseConfig ) where

import           Control.Applicative  hiding (empty)
import           Control.Exception
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as A
import           Data.ByteString      (ByteString)
import           Data.Map             (empty, fromList)
import           Data.Text            hiding (empty, filter, head, map)
import           Data.Yaml.Syck
import           GNS.Data
import           GNS.Trigger          (parseTrigger)
import           Prelude              hiding (not, or)
import           System.Cron
import           System.Cron.Parser
import Control.Arrow

parseConfig :: FilePath -> IO GState
parseConfig file = do
    EMap root <- n_elem <$> parseYamlFile file
    return $ GState (unpackHosts root) (unpackGroups root) (unpackTriggers root) (unpackChecks root)

-----------------------------------------------------------------------------------------
-- helpers
-----------------------------------------------------------------------------------------

unElem :: YamlNode -> Text
unElem = unpackEStr . n_elem

unNode :: [(YamlNode, YamlNode)] -> ByteString -> YamlNode
unNode root node = let [(_, x)] = filter (\(y,_) -> n_elem y == EStr node) root
                   in x

unEN :: [(YamlNode, YamlNode)] -> ByteString -> Text
unEN x = unElem . unNode x

-----------------------------------------------------------------------------------------
-- hosts
-----------------------------------------------------------------------------------------

unpackHosts :: [(YamlNode, YamlNode)] -> [Hostname]
unpackHosts root = map Hostname $ unpackESeq $ unNode root "hosts"

unpackESeq :: YamlNode -> [Text]
unpackESeq x = let ESeq l = n_elem x
               in map unElem l

unpackEStr :: YamlElem -> Text
unpackEStr (EStr z) = pack $ unpackBuf z

-----------------------------------------------------------------------------------------
-- groups
-----------------------------------------------------------------------------------------

unpackGroups :: [(YamlNode, YamlNode)] -> [Group]
unpackGroups root = let ESeq l = n_elem $ unNode root "groups"
                    in map unpackGroup l

unpackGroup :: YamlNode -> Group
unpackGroup group = let EMap ls = n_elem group
                    in Group
                         { name = unEN ls "name"
                         , hosts = map Hostname $ unpackESeq $ unNode ls "hosts"
                         , triggers = unpackESeq $ unNode ls "triggers"
                         }

-----------------------------------------------------------------------------------------
-- triggers
-----------------------------------------------------------------------------------------

unpackTriggers :: [(YamlNode, YamlNode)] -> [Trigger]
unpackTriggers root = let ESeq l = n_elem $ unNode root "triggers"
                      in map unpackTrigger l

unpackTrigger :: YamlNode -> Trigger
unpackTrigger trigger = let EMap ls = n_elem trigger
                            pp = parseTrigger $ unElem $ unNode ls "result"
                            p = case pp of
                                     Left y -> error $ show y
                                     Right y -> y
                        in Trigger
                             { _name = unEN ls "name"
                             , _check = CheckName $ unEN ls "check"
                             , _description = unEN ls "description"
                             , _result = p
                             }

-- @ TODO fix 
parseSchedule :: Text -> CronSchedule
parseSchedule cron = let (Right x) = parseOnly cronSchedule cron -- can fail here
                     in x

-----------------------------------------------------------------------------------------
-- checks
-----------------------------------------------------------------------------------------

unpackChecks :: [(YamlNode, YamlNode)] -> [Check]
unpackChecks root = let ESeq l = n_elem $ unNode root "checks"
                    in map unpackCheck  l

unpackCheck :: YamlNode -> Check
unpackCheck check = let EMap ls = n_elem check
                        checkName = unNode ls "name"
                        period = parseSchedule $  unEN ls "period"
                    in Check 
                        { _checkName = CheckName $ unElem checkName
                        , _period = period 
                        , _params = fromList $ map (unElem *** unElem) $ filter (\(x, _) -> n_elem x /= EStr "name" || n_elem x /= EStr "period") ls
                        }

