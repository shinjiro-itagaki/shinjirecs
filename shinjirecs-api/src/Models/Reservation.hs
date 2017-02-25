{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import DB(Reservation(..))
import DB.Status(ReservationState(..))
import Model(ActiveRecord(..))
-- import qualified Database.Persist.Class as PS
import Database.Persist.Sql(toSqlKey)  --persistent

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
