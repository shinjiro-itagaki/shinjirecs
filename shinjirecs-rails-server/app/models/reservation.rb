class Reservation < ApplicationRecord
  belongs_to :channel
  belongs_to :program_title

  # execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_state CHECK( state IN (-2,-1,0,1,2));"
  # execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_duration CHECK( duration > 0);"
  # execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_duration CHECK( counter >= 0);"

  # -2: canceled
  # -1: failed
  #  0: waiting
  #  1: recording
  #  2: success
  enum state: { waiting: 0, recording: 1, success: 2, failed: -1, canceled: -2 }

  def initialize
    self.state.waiting!
    @weekdays = Weekdays.none
  end

#  def select_label(lmap)
#    lmap[self.status] || lmap[self.status.to_s] || "j"
#  end


  # def duration(from=self.starttime)
  #   ( self.stoptime - from ).to_i
  # end

  # def to_next_week
  #   self + 7
  # end

  def next_reservation
    return nil if not self.has_next?
    r = self + @next.to_i
    7.times do
      ex = self.excluded?(r.starttime.wday)
      ( ex ) ? r += 1 : break
    end if @next.to_i == 1
    r.counter += 1
    r.load_from_program
    r
  end

  def excluded?(wday)
    return false if not @weekdays
    ( @weekdays & ( 0x01 << wday )) == 0
  end

  def recorded?
    self.success? || self.failed?
  end

  def savable?
    return true if Time.now > self.stop_time # done yet
    return false if not (self.starttime < self.stoptime)

    ntuner = self.ch.tuner.sum
    rsvs = self.find_overlapped_reservations

    if (rsvs.size + 1) > ntuner then
      st = self.starttime
      ed = self.stoptime
      nst = rsvs.map{|r|r.starttime}.find_all{|t| t <= ed }.max
      ned = rsvs.map{|r|r.stoptime }.find_all{|t| t >= st }.min
      n = rsvs.find_all{|r|
        r.time_overlapped2?(st,ned)
      }.size + 1

      return false if n > ntuner

      n = rsvs.find_all{|r|
        r.time_overlapped2?(nst,ed)
      }.size + 1

      return false if n > ntuner
    end
    true
  end

  def find_overlapped_reservations
    self.class.find_all.find_all{|rsv|
      self != rsv and rsv.enable? and rsv.ch.tuner == self.ch.tuner and self.time_overlapped?(rsv)
    }
  end

  def ==(other)
    @channel_id == other.channel_id and @start_time == other.start_time
  end


  # def self.update_from_program
  #   self.find_all.each do |rsv|
  #     rsv.save if rsv.load_from_program
  #   end
  # end

  # def load_from_program
  #   p = self.find_program
  #   return false if not p
  #   @title = p.title
  #   @desc  = p.desc
  #   true
  # end

  # def videofilesize(div=2**20)
  #   File.size?(self.videofilepath).to_i / div
  # end

  # def enable!() @disable = false  end
  # def disable!() @disable = true  end
  # def disable?() ( @disable ) ? true : false  end
  # def enable?() !self.disable? end

  # def symlink_filename
  #   fname = self.videofilename
  #   ".__sym__" + Digest::SHA1.hexdigest(fname).to_s + File.extname(fname)
  # end

  # def symlink_filepath
  #   "#{StrageDir}/" + self.symlink_filename
  # end

  # def create_symlink
  #   begin
  #     path = self.symlink_filepath
  #     if File.exist?(path) then
  #       true
  #     else
  #       File.symlink(self.videofilepath,
  #                    path )
  #     end
  #   rescue => e
  #     return false
  #   end
  # end

  # def delete_symlink
  #   path = self.symlink_filepath
  #   (File.exist?(path)) ? FileUtils.rm_f(path) : true
  # end

  # def archive_success?() @archive_result == @@success end
  # def archive_failed?()  @archive_result == @@failed  end
  # def archive_waiting?() @archive_result == @@waiting end

  # def archive_success!() @archive_result = @@success end
  # def archive_failed!()
  #   self.not_archive!
  #   @archive_result = @@failed
  # end
  # def archive_waiting!() @archive_result = @@waiting  end

  # def archivefilesize(div=2**20)
  #   File.size?(self.archivefilepath).to_i / div
  # end

  # def archive_tmp_filesize(div=2**20)
  #   (self.now_archive_creating?) ? File.size?(@tmpfilepath).to_i / div : 0
  # end

#  def everyweek?() @next.to_i == 7 end
#  def everyweek!() @next = 7 ; self; end
#  def not_everyweek!() @next = 0 if @next == 7 ; self; end

#  def exist?() File.exist? self.filepath end
#  def videofileexist?() File.exist? self.videofilepath end
end
