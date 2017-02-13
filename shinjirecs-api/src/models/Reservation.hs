{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import DB(Reservation(..), ReservationStatus(..))
import Model(Record)

instance Record Reservation

-- select_label(lmap)
-- excluded?(wday)
-- self.find(id)
isEveryweek :: Reservation -> Bool
setEveryweek :: Reservation -> Reservation
setNotEveryweek :: Reservation -> Reservation

isEveryweek r = reservationNext r == 7
setEveryweek r = r { reservationNext = 7 }
setNotEveryweek r@Reservation { reservationNext = 7 } = r { reservationNext = 0 }
setNotEveryweek r = r

-- exist?() File.exist? self.filepath end
-- videofileexist?() File.exist? self.videofilepath end
-- status() @status ||= @@waiting end
-- waiting?() self.status == @@waiting  end
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
