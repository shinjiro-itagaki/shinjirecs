{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where

data Status =  Waiting | Recording | Success | Failed deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- select_label(lmap)
-- excluded?(wday)
-- self.find(id)
-- everyweek?() @next.to_i == 7 end
-- everyweek!() @next = 7 ; self; end
-- not_everyweek!() @next = 0 if @next == 7 ; self; end
-- self.find_all() self.find("*") end
-- self.find_by_id(id) self.find(id)[0] end

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
