{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import DB(Reservation(..))
import DB.Status(ReservationState(..))
import Model(ActiveRecord(..))
-- import qualified Database.Persist.Class as PS
import Database.Persist.Sql(toSqlKey)  --persistent
import Data.Time.Clock(UTCTime,getCurrentTime,utctDay)
import Helper(finishTime,inTime,inTimeNow,(.++),weekDayFlagsToWeekDays,nearestWeekDayInterval)

import Data.Dates(WeekDay(..),dateWeekDay,dayToDateTime) -- dates

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
  Just $ r { reservationStartTime = (reservationStartTime r) .++ (toInteger x),
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
