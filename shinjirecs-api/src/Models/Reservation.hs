{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import DB(Reservation(..))
import DB.Status(ReservationState(..))
import Model(ActiveRecord(..))
-- import qualified Database.Persist.Class as PS
import Database.Persist.Sql(toSqlKey)  --persistent
import Data.Time.Clock(UTCTime,getCurrentTime,utctDay)
import Helper(finishTime,inTime,inTimeNow,(.++),weekDayFlagsToWeekDays,nearestWeekDayInterval,DateTime(..), pNum0xd,replaceString)

import Data.Dates(WeekDay(..),dateWeekDay,dayToDateTime) -- dates
import Config(Config(..),PathsConfig(..),ReservationConfig(..))
import System.FilePath.Posix((</>),pathSeparators) -- filepath

instance ActiveRecord Reservation

  -- delete
  -- deleteWhere

-- select_label(lmap)
-- excluded?(wday)
isEveryweek     :: Reservation -> Bool
setEveryweek    :: Reservation -> Reservation
setNotEveryweek :: Reservation -> Reservation
isWaiting       :: Reservation -> Bool
isSameState     :: ReservationState -> Reservation -> Bool

isEveryweek     r = reservationNext r == 7
setEveryweek    r = r { reservationNext = 7 }
setNotEveryweek r@Reservation { reservationNext = 7 } = r { reservationNext = 0 }
setNotEveryweek r = r
-- isVideoFileExist :: Reservation -> Bool ---File.exist? self.videofilepath end
-- isVideoFileExist = 
state       = reservationState
isSameState = impl' where impl' status r = (reservationState r) == status
isWaiting   = isSameState Waiting
isRecording = isSameState Recording
isSuccess   = isSameState Success
isFailed    = isSameState Failed

-- recorded?
-- destroy()
-- videofile_deletable?
-- destroyable?
-- destroy_archive
-- destroy_videofile

-- filepath()
-- filename

-- videoFileNameFormat FilePath

data FormatSymbolDateType = Year | Month | Day | Hour | Minute | Second deriving (Show,Enum,Bounded)
data FormatSymbol = Counter | StartTime FormatSymbolDateType

symbolKeyStr :: FormatSymbol -> String
symbolKeyStr sym = "%{" ++ (show sym) ++ "}"

symbolValue :: Reservation -> FormatSymbol -> String
symbolValue r Counter = reservationCounterStr r
symbolValue r (StartTime tipe) = case tipe of
  Year   -> show    $ fst ymd'
  Month  -> printf' $ fst $ snd ymd'
  Day    -> printf' $ snd $ snd ymd'
  Hour   -> printf' $ fst hms'
  Minute -> printf' $ fst $ snd hms'
  Second -> printf' $ snd $ snd hms'
  where
    printf' :: (Integral a) => a -> String
    printf' = pNum0xd 2 . fromIntegral
    ymd' = toYMD $ reservationStartTime r
    hms' = toHMS $ reservationStartTime r

instance Show FormatSymbol where
  show Counter = "counter"
  show (StartTime tipe) = "start" ++ (show tipe)

allFormatSymbols :: [FormatSymbol]
allFormatSymbols = [Counter] ++ (map StartTime [minBound .. maxBound])

reservationCounterStr :: Reservation -> String
reservationCounterStr r = pNum0xd keta' counter'
  where
    keta' = reservationKeta r
    counter' = reservationCounter r

reservationFileName :: Reservation -> FilePath
reservationFileName r = sanitize' $ foldl replace' (reservationVideoFileNameFormat r) allFormatSymbols
  where
    replace' rtn sym = replaceString (symbolKeyStr sym) (symbolValue r sym) rtn
    sanitizeOne' str separator = replaceString [separator] "_" str
    sanitize' str = foldl sanitizeOne' str pathSeparators

reservationFilePath :: PathsConfig -> Reservation -> FilePath
reservationFilePath pconfig r = (videoFilesDir pconfig) </> reservationFileName r

reservationFinishTime :: Reservation -> UTCTime
reservationFinishTime r = finishTime (reservationStartTime r) (reservationDuration r)

reservationInTime :: Reservation -> UTCTime -> Bool
reservationInTime r t = inTime (reservationStartTime r) (reservationDuration r) t

reservationInTimeNow :: Reservation -> IO Bool
reservationInTimeNow r = inTimeNow (reservationStartTime r) (reservationDuration r)

inTimeReservations :: [Reservation] -> UTCTime -> [Reservation]
inTimeReservations rs t = filter (\r -> reservationInTime r t) rs

inTimeNowReservations :: [Reservation] -> IO [Reservation]
inTimeNowReservations rs = getCurrentTime >>= return . inTimeReservations rs

createNextReservations :: Reservation -> Maybe Reservation
createNextReservations r@Reservation {reservationNext = 0} = Nothing
createNextReservations r@Reservation {reservationNext = x} =
  Just $ r { reservationStartTime = (reservationStartTime r) .++ x,
             reservationNext = calcNext r }

reservationWeekDay :: Reservation -> WeekDay
reservationWeekDay = dateWeekDay . dayToDateTime . utctDay . reservationStartTime

reservationWeekDays :: Reservation -> [WeekDay]
reservationWeekDays Reservation { reservationXwday = xwday } = weekDayFlagsToWeekDays xwday

calcNext :: Reservation
         -> Word -- second
calcNext r = (*) (24 * 3600) $ nearestWeekDayInterval rWeekDay' rWeekDays'
  where
    rWeekDay'  = reservationWeekDay  r
    rWeekDays' = reservationWeekDays r

{-
  startTime UTCTime
  duration Int default=0
  title String
  description Text
  next Word
  name Text
  counter Word
  keta Word
  videoFileNameFormat FilePath
  xwday Word
  state ReservationState
-}
