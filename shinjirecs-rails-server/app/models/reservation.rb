# t.timestamp  "start_time"          , null: false
# t.integer    "duration"            , null: false # seconds
# t.integer    "channel_id"          , null: false , default: 0, foreign_key: {on_delete: :restrict   , on_update: :cascade}
# t.integer    "program_title_id"    , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
# t.string     "title"               , null: false
# t.text       "desc"                , null: false
# t.integer    "event_id"            , null: false , default: 0
# t.integer    "counter"             , null: false , default: 0
# t.integer    "state"               , null: false , default: 0, limit: 1
# t.text       "command_str"         , null: false
# t.integer    "command_pid"         , null: false
# t.text       "log"                 , null: false
# t.text       "errror_log"          , null: false
# t.string     "filename"            , null: false
class Reservation < ApplicationRecord

  belongs_to :channel
  belongs_to :program_title

  # validates :duration, length: { minimum: 1 }, numericality: { only_integer: true }
  minimum :duration, 1
  # validates :counter,  length: { minimum: 0 }, numericality: { only_integer: true }
  minimum :counter, 1

  enum state: { waiting: 0, preparing: 1, recording: 2, success: 3, failed: -1, canceled: -2 }

  @@reservations_cache = {
    waiting:   {},
    preparing: {},
    recording: {},
    success:   {},
    failed:    {},
    canceled:  {}
  }.freeze # :: {Int => {Int => Reservation}}

  def self.set_reservation_cache(rsv)
    @@reservations_cache.keys.each do |k,m|
      m[rsv.id] = nil
    end
    @@reservations_cache[rsv.state]
  end

  scope :will_start_in, ->(sec = 10){
    now ||= Time.now
    # where{ start_time.gt(now) & start_time.lt(now+sec) }
    where("start_time > ?", now).where("start_time < ?", now+sec)
  }

  def will_start_in?(sec)
    now = Time.now
    @start_time > now && @start_time < now + sec
  end

  scope :now_in_time, ->() {
    now = Time.now
    where("start_time <= ?", now ).where("end_time >= ?", now)
  }

  def now_in_time?
    now = Time.now
    @start_time <= now && @end_time >= now
  end

  def self.run
    self.count
    1
  end

  @@do_check_preparing_now = false

  def self.check_preparing
    if @@do_check_preparing_now then
      return false
    else
      @@do_check_preparing_now = true
      proxy = self.waiting.will_start_in(30)
      rsvs = proxy.all.to_a
      # @@staging_reservations[rsv.id] ||= rsv
      rsvs.each do |rsv|
        rsv.state.preparing!
      end

      self.transaction do
        rsvs.each do |rsv|
          rsv.save!
        end
      end
    end
  end

  after_save :after_save_impl

  def after_save_impl
    st = self.class.states
    case @state
    when st[:waiting] then
      self.after_save_state_waiting
    when st[:recording] then
      self.after_save_state_recording
    when st[:success] then
      self.after_save_state_success
    when st[:failed] then
      self.after_save_state_failed
    when st[:canceled] then
      self.after_save_state_canceled
    end
    self.class.check_preparing
  end

  def after_save_state_waiting
  end

  def after_save_state_recording
  end

  def after_save_state_success
  end

  def after_save_state_failed
  end

  def after_save_state_canceled
  end

  after_initialize :set_default_value

  def set_default_value
    self.waiting! if not self.state
    @weekdays ||= Weekdays.none
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

  def recording_tried?
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

