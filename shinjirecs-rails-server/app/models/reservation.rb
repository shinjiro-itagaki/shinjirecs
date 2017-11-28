# t.timestamp  "start_time"          , null: false
# t.integer    "stop_time"           , null: false # seconds
# t.integer    "channel_id"          , null: false , default: 0, foreign_key: {on_delete: :restrict   , on_update: :cascade}
# t.integer    "program_series_id"    , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
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
  require "securerandom"

  belongs_to :channel
  belongs_to :program_series

  minimum :counter, 1

  enum state: { waiting: 0, preparing: 1, recording: 2, success: 3, failed: -1, canceled: -2 }

  validate do |rec|
    rec.errors[:stop_time] << "inconsistent stop_time" if not rec.start_time < rec.stop_time
    rec.errors[:stop_time] << "this reservation will not record" if not Time.now < rec.stop_time
    rec.errors[:start_time] << "will not use tuner,because count of overrapped reservations is over than tuner's count" if not rec.will_recordable?
  end

  scope :will_use_tuner, ->(){ where(state: [:waiting, :preparing, :recording]) }

  scope :future_stop_time, -> (){
    where(["stop_time > ?",Time.now])
  }

  scope :in_ctype, -> (ctype){
    joins(:channel).where(["#{Channel.table_name}.ctype = ?", ctype])
  }

  scope :will_record, -> {
    self.will_use_tuner.future_stop_time
  }

  before_create do
    self.filename = SecureRandom.hex(10) + ".ts"
  end

  before_save do
    self.desc ||= ""
    self.command_str ||= ""
    self.log ||= ""
    self.error_log ||= ""
  end

  def self.video_dir
    Rails.application.config.video_path
  end

  def self.select_overlapped_proxy(st,ed,ctype,exclude_ids=[])
    proxy = self.in_ctype(ctype)
      .where(["not (start_time > ? or stop_time < ?)", ed, st])
    if not exclude_ids.empty? then
      proxy = proxy.where.not(id: exclude_ids)
    end
    proxy
  end

  def self.max_overlapped_count(at_list, rsvlist, max=0)
    if at_list.empty? or rsvlist.empty? then
      return max
    end
    at = at_list.shift
    new_rsvlist = rsvlist.delete_if{|r|r.over?(at)}
    new_max = [max, rsvlist.select{|r| r.in_time?(at) }.map(&:id).uniq.count].max
    self.max_overlapped_count(at_list, new_rsvlist, new_max)
  end

  def self.tuner_count_changeable?(ctype,count)
    rsvs = self.will_record.in_ctype(ctype).to_a
    allat = rsvs.map{|x| [x.start_time, x.stop_time] }.flatten.sort.uniq
    self.max_overlapped_count(allat,rsvs) <= count
  end

  def filepath
    self.class.video_dir + "./" + self.filename
  end

  def file
    File.exists?(paht = self.filepath) ? File.new(path) : nil
  end

  def select_overlapped_proxy(exclude_self=true)
    ex_ids = (self.persisted? and exclude_self) ? [self.id] : []
    self.class.select_overlapped_proxy(self.stop_time, self.start_time, self.channel.ctype, ex_ids)
  end

  def will_use_tuner_count()
    self.select_overlapped_proxy.will_use_tuner.count
  end

  def will_recordable?
    if self.new_record?
      self.will_recordable_if_new?
    else
      self.will_recordable_if_persisted?
    end
  end

  def will_recordable_if_new?
    self.will_use_tuner_count < self.tuner_count
  end

  def will_recordable_if_persisted?
    tuner_count = self.tuner_count
    list = self.select_overlapped_proxy(exclude_self = false)
      .will_use_tuner
      .order(start_time: :asc)
      .limit(tuner_count)
      .where(id: self.id)
      .pluck(:id)
    list.include? self.id
  end

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
    self.start_time > now && self.start_time < now + sec
  end

  scope :now_in_time, ->() {
    now = Time.now
    where("start_time <= ?", now ).where("stop_time >= ?", now)
  }

  def now_in_time?
    self.in_time? Time.now
  end

  def in_time?(at)
    (self.start_time <= at) && (at <= self.stop_time)
  end

  def over?(at)
    at >= self.stop_time
  end

  def now_over?(at)
    self.over?(Time.now)
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

      # 1. state is waiting and process id is none or the written is not found
      # 2. state is recording but process id is none or the written is not found
      # 3. state is 
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

  after_create_commit :after_create_commit_impl

  def after_create_commit_impl
    if self.now_in_time? then
      puts "now in time"
    else
      puts "jiojo"
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
  end

  # sec
  def duration_sec
    ( self.stop_time - self.start_time ).to_i
  end

  def duration_min
    ( self.duration_sec / 60 ).to_i
  end

  def duration
    self.duration_sec
  end

  # def recording_tried?
  #   self.success? || self.failed?
  # end

  # def videofilesize(div=2**20)
  #   File.size?(self.videofilepath).to_i / div
  # end

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

#  def everyweek?() @next.to_i == 7 end
#  def everyweek!() @next = 7 ; self; end
#  def not_everyweek!() @next = 0 if @next == 7 ; self; end

#  def exist?() File.exist? self.filepath end
  #  def videofileexist?() File.exist? self.videofilepath end

end

