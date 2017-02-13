{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import DB(Reservation(..))
import DB.Status(ReservationState(..))
import Model(Record)

instance Record Reservation

-- select_label(lmap)
-- excluded?(wday)
-- self.find(id)
isEveryweek :: Reservation -> Bool
setEveryweek :: Reservation -> Reservation
setNotEveryweek :: Reservation -> Reservation
isWaiting :: Reservation -> Bool
  
isEveryweek r = reservationNext r == 7
setEveryweek r = r { reservationNext = 7 }
setNotEveryweek r@Reservation { reservationNext = 7 } = r { reservationNext = 0 }
setNotEveryweek r = r
-- isVideoFileExist :: Reservation -> Bool ---File.exist? self.videofilepath end
-- isVideoFileExist = 
-- status :: () @status ||= @@waiting end

isWaiting r = (reservationState r) == Waiting
-- recording?() self.status == @@recording  end
-- success?() self.status == @@success  end
-- failed?() self.status == @@failed  end
-- recorded?
-- destroy()
-- videofile_deletable?
-- destroyable?
-- destroy_archive
-- destroy_videofile

-- filepath()
-- filename
