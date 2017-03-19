{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Helper where
import Data.Bits(shiftL,(.&.),(.|.))
import Data.Bool(Bool,not)
import Data.Maybe(isJust)
import Database.Persist.Class(PersistEntity)
import Data.Time.Clock(UTCTime,addUTCTime,NominalDiffTime,getCurrentTime)
import Data.Word(Word)
import Data.Dates(WeekDay(..),weekdayNumber,intToWeekday) -- dates

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


(.++) :: UTCTime -> Integer -> UTCTime
(.++) t sec = (fromInteger sec :: NominalDiffTime) `addUTCTime` t


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
