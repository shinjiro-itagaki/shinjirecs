{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Class.DateTime where
import Data.Time.Clock(UTCTime(..))
import Data.Time.Calendar(fromGregorian,toGregorian)

class DateTimeClass a where
  toYMD :: a -> (Integer, (Word, Word))
  toHMS :: a -> (Word   , (Word, Word))
  year :: a -> Integer
  mon,month,day,hour,min,minute,sec,second :: a -> Word
  year   = fst . toYMD
  month  = fst . snd . toYMD
  mon = month  
  day    = snd . snd . toYMD
  hour   = fst . toHMS
  minute = fst . snd . toHMS
  min = minute  
  second = snd . snd . toHMS
  sec = second
  
instance DateTimeClass UTCTime where
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
