{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper where
import Data.Bits(shiftL,(.&.),(.|.))
import Data.Bool(Bool,not)
import Data.Maybe(isJust)
import Database.Persist.Class(PersistEntity)
import Data.Text(replace,pack,unpack)
import Data.Time.Clock(UTCTime(..),addUTCTime,NominalDiffTime,getCurrentTime)
import Data.Time.Calendar(fromGregorian,toGregorian)
import Data.Word(Word)
import Data.Dates(WeekDay(..),weekdayNumber,intToWeekday) -- dates
import Text.Printf(printf,PrintfType)

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
weekdayInterval from to =
  let interval' = (weekdayNumber to) - (weekdayNumber from)
      max' = weekdayNumber (maxBound ::  WeekDay)
  in
    fromInteger $ toInteger (if interval' > 0 
                             then interval'
                             else max' + interval')

-- 2nd argument is empty means next week
nearestWeekDay :: WeekDay -> [WeekDay] -> WeekDay
nearestWeekDay from []  = from
nearestWeekDay from wds = intToWeekday $ fromInteger $ toInteger $ minimum $ map (weekdayInterval from) wds

nearestWeekDayInterval :: WeekDay -> [WeekDay] -> UInt
nearestWeekDayInterval from [] = 0
nearestWeekDayInterval from xs = weekdayInterval from $ nearestWeekDay from xs

allWeekDays ::[WeekDay]
allWeekDays = [minBound .. maxBound]

-- Monday    => 0b0000001
-- Tuesday   => 0b0000010
-- Wednesday => 0b0000100
-- Thursday  => 0b0001000
-- Friday    => 0b0010000
-- Saturday  => 0b0100000
-- Sunday    => 0b1000000
weekDayToMask :: WeekDay -> UInt
weekDayToMask wd = 1 `shiftL` (weekdayNumber wd)

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

-- time = from (2017,3,21)
-- (y,m,d) = cast time
class Castable b a where
  cast :: a -> b
  from :: b -> a

instance (Integral y, Integral m, Integral d, Integral hh, Integral mm, Integral ss) => Castable (y,m,d,hh,mm,ss) UTCTime where
  cast t = (fromIntegral $ year t,
            fromIntegral $ month t,
            fromIntegral $ day t,
            fromIntegral $ hour t,
            fromIntegral $ minute t,
            fromIntegral $ second t)
  from (y,m,d,hh,mm,ss) = addUTCTime hms' $ UTCTime day' $ fromInteger 0
    where
      day' = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
      hms' = fromInteger $ (3600 * toInteger hh) + (60 * toInteger mm) + (toInteger ss)

instance (Integral y, Integral m, Integral d, Integral hh, Integral mm) => Castable (y,m,d,hh,mm) UTCTime where
  cast t = (fromIntegral $ year t,
            fromIntegral $ month t,
            fromIntegral $ day t,
            fromIntegral $ hour t,
            fromIntegral $ minute t)
  from (y,m,d,hh,mm) = from (y,m,d,hh,mm,0)

instance (Integral y, Integral m, Integral d, Integral hh) => Castable (y,m,d,hh) UTCTime where
  cast t = (fromIntegral $ year t,
            fromIntegral $ month t,
            fromIntegral $ day t,
            fromIntegral $ hour t)
  from (y,m,d,hh) = from (y,m,d,hh,0)

instance (Integral y, Integral m, Integral d) => Castable (y,m,d) UTCTime where
  cast t = (fromIntegral $ year t,
            fromIntegral $ month t,
            fromIntegral $ day t)
  from (y,m,d) = from (y,m,d,0)  


instance (Integral y, Integral m) => Castable (y,m) UTCTime where
  cast t = (fromIntegral $ year t,
            fromIntegral $ month t)
  from (y,m) = from (y,m,1 :: Word)
