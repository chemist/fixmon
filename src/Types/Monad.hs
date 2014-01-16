{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Monad where

import           Types.Shared

import           Control.Monad.Error      (ErrorT, MonadError, runErrorT)
import           Control.Monad.RWS.Strict (MonadIO, MonadRWS, MonadReader,
                                           MonadState, MonadWriter, RWST,
                                           runRWST)

newtype Gns a = Gns {run:: ErrorT String (RWST StartOptions Log Monitoring IO) a} deriving
  ( Monad
  , MonadIO
  , Functor
  , MonadError String
  , MonadReader StartOptions
  , MonadState Monitoring
  , MonadWriter Log
  , MonadRWS StartOptions Log Monitoring
  )

flip' :: (ErrorT String (RWST StartOptions Log Monitoring IO) a -> b -> c -> d) -> b -> c -> Gns a -> d
flip' f a b c =  f (run c) a b

runGns :: StartOptions -> Monitoring -> Gns a -> IO (Either String a, Monitoring, Log)
runGns = flip' $ runRWST . runErrorT




