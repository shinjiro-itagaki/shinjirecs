{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper.DateTimeHelper where
import Class.Castable
import Data.Word(Word)
import Data.Time.Clock(UTCTime,addUTCTime,NominalDiffTime,getCurrentTime)
import Data.Dates(WeekDay(..),weekdayNumber,intToWeekday) -- dates
import Class.DateTime(DateTimeClass(..))
import Helper.EnumHelper(all,enumToMask)
import Data.Bits((.|.),(.&.))
import Data.List(sortBy)

(.++) :: Integral a => UTCTime -> a -> UTCTime
(.++) t sec = ((fromInteger $ toInteger sec) :: NominalDiffTime) `addUTCTime` t

finishTime :: UTCTime -> Word -> UTCTime
finishTime t d = t .++ (toInteger d)

inTime :: UTCTime -- start time
       -> Word    -- duration
       -> UTCTime -- object time
       -> Bool
inTime st d t = st <= t && t <= (finishTime st d)

inTimeNow :: UTCTime -> Word -> IO Bool
inTimeNow st d = getCurrentTime >>= return . (inTime st d)


-- interval between same weekday means 1 week later
weekdayInterval :: WeekDay -> WeekDay -> Word
weekdayInterval from to = fromInteger $ toInteger (if interval' > 0 
                                                   then interval'
                                                   else 7 + interval')
  where
    interval' = (weekdayNumber to) - (weekdayNumber from)

-- 2nd argument is empty means next week
nearestWeekDay :: WeekDay -> [WeekDay] -> WeekDay
nearestWeekDay x []  = x
nearestWeekDay x wds = head $ sortBy (\l r -> weekdayInterval' l `compare` weekdayInterval' r) wds
  where
    weekdayInterval' = weekdayInterval x

nearestWeekDayInterval :: WeekDay -> [WeekDay] -> Word
nearestWeekDayInterval x [] = 0
nearestWeekDayInterval x xs = weekdayInterval x $ nearestWeekDay x xs

allWeekDays ::[WeekDay]
allWeekDays = Helper.EnumHelper.all

weekdays = [Monday,Tuesday,Wednesday,Thursday,Friday]
holidays = [Saturday,Sunday]

-- Monday    => 0b0000001
-- Tuesday   => 0b0000010
-- Wednesday => 0b0000100
-- Thursday  => 0b0001000
-- Friday    => 0b0010000
-- Saturday  => 0b0100000
-- Sunday    => 0b1000000
weekDayToMask :: WeekDay -> Word
weekDayToMask = enumToMask

weekDaysToFlags :: [WeekDay] -> Word
weekDaysToFlags = foldl (.|.) 0 . map weekDayToMask

weekDayFlagsToWeekDays :: Word -> [WeekDay]
weekDayFlagsToWeekDays 0     = []
weekDayFlagsToWeekDays flags = filter (weekDayFlagIsOn flags) allWeekDays

weekDayFlagIsOn :: Word -> WeekDay -> Bool
weekDayFlagIsOn flags wd = not $ (flags .&. (weekDayToMask wd)) == 0

timeToTuple :: (Integral y, Integral m, Integral d, Integral hh, Integral mm, Integral ss) => UTCTime -> (y,m,d,hh,mm,ss)
timeToTuple t = (fromIntegral $ year t
                , fromIntegral $ month t
                , fromIntegral $ day t
                , fromIntegral $ hour t
                , fromIntegral $ minute t
                , fromIntegral $ second t)


instance Castable [WeekDay] Word where
  from = weekDaysToFlags
  
instance Castable Word [WeekDay] where
  from = weekDayFlagsToWeekDays

instance Castable WeekDay Word where
  from = weekDayToMask

-- (y,m,d,hh,mm,ss) = from time
instance (Integral y, Integral m, Integral d, Integral hh, Integral mm, Integral ss) => Castable UTCTime (y,m,d,hh,mm,ss) where
  from = timeToTuple

-- (y,m,d,hh,mm) = from time
instance (Integral y, Integral m, Integral d, Integral hh, Integral mm) => Castable UTCTime (y,m,d,hh,mm) where
  from = (\(y,m,d,hh,mm,ss) -> (y,m,d,hh,mm)) . timeToTuple

-- (y,m,d,hh) = from time
instance (Integral y, Integral m, Integral d, Integral hh) => Castable UTCTime (y,m,d,hh) where
  from = (\(y,m,d,hh,mm,ss) -> (y,m,d,hh)) . timeToTuple

-- (y,m,d) = from time
instance (Integral y, Integral m, Integral d) => Castable UTCTime (y,m,d) where
  from = (\(y,m,d,hh,mm,ss) -> (y,m,d)) . timeToTuple

-- (y,m) = from time
instance (Integral y, Integral m) => Castable UTCTime (y,m) where
  from = (\(y,m,d,hh,mm,ss) -> (y,m)) . timeToTuple
