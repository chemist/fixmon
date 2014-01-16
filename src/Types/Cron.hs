{-# LANGUAGE DeriveDataTypeable #-}
module Types.Cron
( Cron(..) )
where

import           Control.Applicative ((<$>), (<*>))
import           Data.Binary         (Binary, get, getWord8, put, putWord8)
import           Data.Typeable       (Typeable)
import           System.Cron

newtype Cron = Cron CronSchedule deriving (Show, Eq, Typeable)

instance Ord Cron where
    compare x y = show x `compare` show y

instance Binary CronField where
    put (Star) = putWord8 0
    put (SpecificField x) = putWord8 1 >> put x
    put (RangeField x y) = putWord8 2 >> put x >> put y
    put (ListField x) = putWord8 3 >> put x
    put (StepField x y) = putWord8 4 >> put x >> put y

    get = do x <- getWord8
             case x of
                  0 -> return Star
                  1 -> SpecificField <$> get
                  2 -> RangeField <$> get <*> get
                  3 -> ListField <$> get
                  4 -> StepField <$> get <*> get
                  _ -> fail "bad binary"

instance Binary Cron where
    put (Cron (CronSchedule (Minutes a) (Hours b) (DaysOfMonth c) (Months d) (DaysOfWeek e))) = put a >> put b >> put c >> put d >> put e
    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        return $ Cron (CronSchedule (Minutes a) (Hours b) (DaysOfMonth c) (Months d) (DaysOfWeek e))


