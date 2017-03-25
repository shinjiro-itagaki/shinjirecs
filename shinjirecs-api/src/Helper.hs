{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper where
import Data.List(sortBy)
import Data.Bits(shiftL,(.&.),(.|.),Bits)
import Data.Bool(Bool,not)
import Data.Maybe(isJust)
import Database.Persist.Class(PersistEntity)
import Data.Text(replace,pack,unpack)
import Data.Time.Clock(UTCTime(..),addUTCTime,NominalDiffTime,getCurrentTime)
import Data.Time.Calendar(fromGregorian,toGregorian)
import Data.Word(Word)
import Data.Dates(WeekDay(..),weekdayNumber,intToWeekday) -- dates
import Text.Printf(printf,PrintfType)
import Control.Monad.IO.Class(MonadIO) -- base
import System.Process(CreateProcess,createProcess,proc)
import Class.Castable

type UInt = Word

class ResultClass a r where
  -- please implement
  toResult :: Bool -> a -> r
  
  toSuccess :: a -> r
  toSuccess = toResult True
  toFailed  :: a -> r
  toFailed = toResult False
  
  -- please implement
  returnValue :: r -> a

  -- please implement
--  isSuccess :: r -> a -> Bool
  isSuccess :: r -> a -> Bool
  
  isFailed :: r -> a -> Bool
  isFailed r = not . (isSuccess r)
  
(=<<&&.) :: (Monad m, ResultClass a r) => (a -> m r) -> (a -> m r) -> (a -> m r)
(=<<&&.) f1 f2 = (\arg -> f1 arg >>= (\res -> let arg2 = returnValue res in if isSuccess res arg2 then f2 arg2 else return res))

infixr 8 =<<&&.

(.&&>>=) :: (Monad m, ResultClass a r) => (a -> m r) -> (a -> m r) -> (a -> m r)
(.&&>>=) f1 f2 = f2 =<<&&. f1
infixl 8 .&&>>=

(<||>) :: (Monad m, ResultClass a r) =>
           (a -> m r) -- on success
        -> (a -> m a) -- on failed
        -> (Bool -> a -> m r) -- result
(<||>) onSuccess onFailed =
  \flag rtn ->
  if flag
  then onSuccess rtn
  else onFailed rtn
       >>= (\rtn2 -> return $ toSuccess rtn2)

infix 7 <||>

(.||>>=) :: (Monad m, ResultClass a r) =>
           (a -> m r) -- f1
        -> (Bool -> a -> m r) -- f2 (do success or failed)
        -> (a -> m r)
          
(.||>>=) f1 f2 = (\arg -> f1 arg >>= (\res -> f2 (isSuccess res arg) (returnValue res)))
    
    
infixl 8 .||>>=

(=<<||.) :: (Monad m, ResultClass a r) =>
            (Bool -> a -> m r)
         -> (a -> m r)
         -> (a -> m r)
(=<<||.) f2 f1 = f1 .||>>= f2


(.++) :: Integral a => UTCTime -> a -> UTCTime
(.++) t sec = ((fromInteger $ toInteger sec) :: NominalDiffTime) `addUTCTime` t

finishTime :: UTCTime -> UInt -> UTCTime
finishTime t d = t .++ (toInteger d)

inTime :: UTCTime -- start time
       -> UInt    -- duration
       -> UTCTime -- object time
       -> Bool
inTime st d t = st <= t && t <= (finishTime st d)

inTimeNow :: UTCTime -> UInt -> IO Bool
inTimeNow st d = getCurrentTime >>= return . (inTime st d)


-- interval between same weekday means 1 week later
weekdayInterval :: WeekDay -> WeekDay -> UInt
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

nearestWeekDayInterval :: WeekDay -> [WeekDay] -> UInt
nearestWeekDayInterval x [] = 0
nearestWeekDayInterval x xs = weekdayInterval x $ nearestWeekDay x xs

all ::(Enum a, Bounded a) => [a]
all = [minBound .. maxBound]

allWeekDays ::[WeekDay]
allWeekDays = Helper.all

weekdays = [Monday,Tuesday,Wednesday,Thursday,Friday]
holidays = [Saturday,Sunday]

enumToMask :: (Enum a, Num b, Bits b) => a -> b
enumToMask x = fromInteger $ 1 `shiftL` (fromEnum x)

-- Monday    => 0b0000001
-- Tuesday   => 0b0000010
-- Wednesday => 0b0000100
-- Thursday  => 0b0001000
-- Friday    => 0b0010000
-- Saturday  => 0b0100000
-- Sunday    => 0b1000000
weekDayToMask :: WeekDay -> UInt
weekDayToMask = enumToMask

weekDaysToFlags :: [WeekDay] -> UInt
weekDaysToFlags = foldl (.|.) 0 . map weekDayToMask

weekDayFlagsToWeekDays :: UInt -> [WeekDay]
weekDayFlagsToWeekDays 0     = []
weekDayFlagsToWeekDays flags = filter (weekDayFlagIsOn flags) allWeekDays

weekDayFlagIsOn :: UInt -> WeekDay -> Bool
weekDayFlagIsOn flags wd = not $ (flags .&. (weekDayToMask wd)) == 0

class DateTime a where
  toYMD :: a -> (Integer, (UInt, UInt))
  toHMS :: a -> (UInt   , (UInt, UInt))
  year :: a -> Integer
  mon,month,day,hour,min,minute,sec,second :: a -> UInt
  year   = fst . toYMD
  month  = fst . snd . toYMD
  mon = month  
  day    = snd . snd . toYMD
  hour   = fst . toHMS
  minute = fst . snd . toHMS
  min = minute  
  second = snd . snd . toHMS
  sec = second
  
instance DateTime UTCTime where
  toYMD = toYMD' . toGregorian . utctDay
    where
      toYMD' (y,m,d) = (y, (fromIntegral m, fromIntegral d))
   
  toHMS t = (h',(m',s'))
    where
      hsec' = fromIntegral 3600
      msec' = fromIntegral 60
      hmin' = fromIntegral 60
      sec' = floor $ toRational $ utctDayTime t
      h' = fromIntegral $ floor $ (fromIntegral sec') / hsec'
      m' = fromIntegral $ (floor $ ((fromIntegral sec') / msec')) `mod` hmin'
      s' = fromIntegral $ (fromIntegral sec') `mod` (fromIntegral 60)
      

pNum0xd :: UInt -> UInt -> String
pNum0xd 0    x = pNum0xd 1 x
pNum0xd keta x = printf ("%0" ++ (show keta) ++ "d") x

replaceString :: String -> String -> String -> String
replaceString old new obj = unpack $ replace (pack old) (pack new) (pack obj)

timeToTuple :: (Integral y, Integral m, Integral d, Integral hh, Integral mm, Integral ss) => UTCTime -> (y,m,d,hh,mm,ss)
timeToTuple t = (fromIntegral $ year t
                , fromIntegral $ month t
                , fromIntegral $ day t
                , fromIntegral $ hour t
                , fromIntegral $ minute t
                , fromIntegral $ second t)

instance Castable [WeekDay] UInt where
  from = weekDaysToFlags
  
instance Castable UInt [WeekDay] where
  from = weekDayFlagsToWeekDays

instance Castable WeekDay UInt where
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

(|||) :: Maybe a -> Maybe a -> Maybe a
(|||) Nothing  Nothing = Nothing
(|||) x Nothing = x
(|||) Nothing y = y
(|||) x y       = x
