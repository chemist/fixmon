{-# LANGUAGE OverloadedStrings #-}
module GNS.Parser where

import           Control.Applicative  hiding (empty)
import           Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as A
import           Data.Map             (empty)
import           Data.Text            hiding (empty, filter, head, map)
import           Data.Yaml.Syck
import           Debug.Trace
import           GNS.Data
import           GNS.Fun              (parseFun)
import           Prelude              hiding (not, or)
import           System.Cron
import           System.Cron.Parser

main' = do
    EMap root <- n_elem <$> parseYamlFile "gnc.yaml"
    print $ unpackHosts root
    putStrLn ""
    print $ unpackGroups root
    putStrLn ""
    print $ unpackChecks root
    return $ unpackTriggers root


-----------------------------------------------------------------------------------------
-- hosts
-----------------------------------------------------------------------------------------

unpackHosts :: [(YamlNode, YamlNode)] -> [Text]
unpackHosts root = let [(_,hosts)] = filter (\(x,y) -> n_elem x == EStr "hosts") root
                   in unpackESeq hosts

unpackESeq :: YamlNode -> [Text]
unpackESeq x = let ESeq l = n_elem x
               in map (unpackEStr . n_elem) l

unpackEStr :: YamlElem -> Text
unpackEStr (EStr z) = pack $ unpackBuf z

-----------------------------------------------------------------------------------------
-- groups
-----------------------------------------------------------------------------------------

unpackGroups :: [(YamlNode, YamlNode)] -> [Group]
unpackGroups root = let [(_, groups)] = filter (\(x,_) -> n_elem x == EStr "groups") root
                        ESeq l = n_elem groups
                    in map unpackGroup l

unpackGroup :: YamlNode -> Group
unpackGroup group = let EMap ls = n_elem group
                        [(_, groupName)] = filter (\(x,_) -> n_elem x == EStr "name") $ ls
                        [(_, groupHosts)] = filter (\(x,_) -> n_elem x == EStr "hosts") $ ls
                        [(_, groupTriggers)] = filter (\(x,_) -> n_elem x == EStr "triggers") $ ls
                    in Group
                         { name = (unpackEStr . n_elem) groupName
                         , hosts = unpackESeq groupHosts
                         , triggers = unpackESeq groupTriggers
                         }

-----------------------------------------------------------------------------------------
-- triggers
-----------------------------------------------------------------------------------------

unpackTriggers :: [(YamlNode, YamlNode)] -> [Trigger]
unpackTriggers root = let [(_, triggers)] = filter (\(x,_) -> n_elem x == EStr "triggers") root
                          ESeq l = n_elem triggers
                      in map unpackTrigger l

unpackTrigger :: YamlNode -> Trigger
unpackTrigger trigger = let EMap ls = n_elem trigger
                            [(_, triggerName)] = filter (\(x,_) -> n_elem x == EStr "name") $ ls
                            [(_, triggerPeriod)] = filter (\(x,_) -> n_elem x == EStr "period") $ ls
                            [(_, triggerCheck)] = filter (\(x,_) -> n_elem x == EStr "check") $ ls
                            [(_, triggerDescription)] = filter (\(x,_) -> n_elem x == EStr "description") $ ls
                            [(_, triggerResult)] = filter (\(x,_) -> n_elem x == EStr "result") $ ls
                            pp = parseFun $ (unpackEStr . n_elem) triggerResult
                            p = case pp of
                                     Left y -> error $ show y
                                     Right y -> y
                        in Trigger
                             { _name = (unpackEStr . n_elem) triggerName
                             , _period = parseSchedule $  (unpackEStr . n_elem) triggerPeriod
                             , _check = (unpackEStr . n_elem) triggerCheck
                             , _description = (unpackEStr . n_elem) triggerDescription
                             , _result = p
                             }



parseSchedule :: Text -> CronSchedule
parseSchedule cron = let (Right x) = parseOnly cronSchedule cron -- fail here
                     in x

-----------------------------------------------------------------------------------------
-- checks
-----------------------------------------------------------------------------------------

unpackChecks :: [(YamlNode, YamlNode)] -> [Check]
unpackChecks root = let [(_, checks)] = filter (\(x,_) -> n_elem x == EStr "checks") root
                        ESeq l = n_elem checks
                    in map unpackCheck  l

unpackCheck :: YamlNode -> Check
unpackCheck check = let EMap ls = n_elem check
                        [(_, checkType)] = filter (\(x, _) -> n_elem x == EStr "type") $ ls
                    in makeCheck ls $ (unpackEStr . n_elem) checkType

makeCheck :: [(YamlNode, YamlNode)] -> Text -> Check
makeCheck ls "httpByStatus" =
  let [(_, checkName)] = filter (\(x, _) -> n_elem x == EStr "name") $ ls
      [(_, checkUrl)] = filter (\(x, _) -> n_elem x == EStr "url") $ ls
  in HttpByStatus ((unpackEStr . n_elem) checkName) ((unpackEStr . n_elem) checkUrl) (Complex empty)
makeCheck ls "shell" =
  let [(_, checkName)] = filter (\(x, _) -> n_elem x == EStr "name") $ ls
      [(_, checkCommand)] = filter (\(x, _) -> n_elem x == EStr "command") $ ls
  in Shell ((unpackEStr . n_elem) checkName) ((unpackEStr . n_elem) checkCommand)  (Complex empty)
