{-# LANGUAGE OverloadedStrings #-}
module GNS.Parser ( parseConfig ) where

import Control.Applicative ((<$>))
import           Data.Attoparsec.Text (parseOnly)
import           Data.ByteString      (ByteString)
import           Data.Map             (fromList)
import           Data.Text            hiding (empty, group, filter, head, map)
import           Data.Yaml.Syck
import           GNS.Data
import           GNS.Trigger          (parseTrigger)
import           Prelude              hiding (not, or)
import           System.Cron.Parser
import Control.Arrow
import Control.Monad.RWS 
import Control.Exception 
import Control.Monad.Error
import Data.Either

parseConfig :: Gns ()
parseConfig = do
    s <- ask
    st <- get
    result <- liftIO $ (try $ do
        EMap root <- n_elem <$> parseYamlFile (config s) 
        che <- mapM runErrorT $ unpackChecks root
        tr <- mapM runErrorT $ unpackTriggers root
        return $ st { _raw = GState (unpackHosts root) (unpackGroups root) (rights tr) (rights che) }) :: Gns (Either SomeException Monitoring)
    either (\e -> throwError $ "cant read config " ++ show e) fun result
    where
    fun :: Monitoring -> Gns ()
    fun f = tell "success read and parse config\n" >> put f

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
unpackEStr _ = throw $ PError "error in unpackEStr, must be EStr"

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

unpackTriggers :: Monad m => [(YamlNode, YamlNode)] -> [ErrorT PError m Trigger]
unpackTriggers root = let ESeq l = n_elem $ unNode root "triggers"
                      in map unpackTrigger l

unpackTrigger :: Monad m => YamlNode -> ErrorT PError m Trigger
unpackTrigger trigger = let EMap ls = n_elem trigger
                            pp = parseTrigger $ unElem $ unNode ls "result"
                        in case pp of
                                Left e -> throw $ PError $ "cant parse trigger " ++ show e
                                Right y -> return $ Trigger
                                    { _name = unEN ls "name"
                                    , _check = CheckName $ unEN ls "check"
                                    , _description = unEN ls "description"
                                    , _result = y
                                    }

-----------------------------------------------------------------------------------------
-- checks
-----------------------------------------------------------------------------------------

unpackChecks :: Monad m => [(YamlNode, YamlNode)] -> [ErrorT PError m Check]
unpackChecks root = let ESeq l = n_elem $ unNode root "checks"
                    in map unpackCheck  l

unpackCheck :: Monad m => YamlNode -> ErrorT PError m Check
unpackCheck check = let EMap ls = n_elem check
                        checkName = unNode ls "name"
                        period = parseOnly cronSchedule $  unEN ls "period"
                    in case period of
                            Left e -> throw $ PError $ "cant parse cron schedule " ++ show e
                            Right r -> return $ Check 
                              { _checkName = CheckName $ unElem checkName
                              , _period = Cron r
                              , _params = fromList $ map (unElem *** unElem) $ filter (\(x, _) -> n_elem x /= EStr "name" || n_elem x /= EStr "period") ls
                              }

