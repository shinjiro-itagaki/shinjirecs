{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Class.Castable where
import System.Process(CreateProcess,createProcess,proc)
import Data.Dates(WeekDay(..),weekdayNumber,intToWeekday) -- dates
import Data.Time.Clock(UTCTime(..),addUTCTime,NominalDiffTime,getCurrentTime)
import Data.Word(Word)
import Control.Monad.IO.Class(MonadIO) -- base
import Data.Time.Calendar(fromGregorian,toGregorian)

-- time = from (2017,3,21)
-- (y,m,d) = from time
class Castable b a where
  from :: b -> a

-- time = from (2017,3,21,22,33,45) :: UTCTime
instance (Integral y, Integral m, Integral d, Integral hh, Integral mm, Integral ss) => Castable (y,m,d,hh,mm,ss) UTCTime where
  from (y,m,d,hh,mm,ss) = addUTCTime hms' $ UTCTime day' $ fromInteger 0
    where
      day' = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
      hms' = fromInteger $ (3600 * toInteger hh) + (60 * toInteger mm) + (toInteger ss)

-- time = from (2017,3,21,22,33) :: UTCTime
instance (Integral y, Integral m, Integral d, Integral hh, Integral mm) => Castable (y,m,d,hh,mm) UTCTime where
  from (y,m,d,hh,mm) = from (y,m,d,hh,mm,0)

-- time = from (2017,3,21,22) :: UTCTime
instance (Integral y, Integral m, Integral d, Integral hh) => Castable (y,m,d,hh) UTCTime where
  from (y,m,d,hh) = from (y,m,d,hh,0)

-- time = from (2017,3,21) :: UTCTime
instance (Integral y, Integral m, Integral d) => Castable (y,m,d) UTCTime where
  from (y,m,d) = from (y,m,d,0)  

-- time = from (2017,3) :: UTCTime
instance (Integral y, Integral m) => Castable (y,m) UTCTime where
  from (y,m) = from (y,m,1)

instance (MonadIO m) => Castable [m a] (m [a]) where
  from [] = return []
  from (mx:mxs) = do
    x <- mx
    xs <- from mxs
    return ([x] ++ xs)

instance Castable (FilePath,[String]) CreateProcess where
  from (scriptpath,args) = proc scriptpath args
