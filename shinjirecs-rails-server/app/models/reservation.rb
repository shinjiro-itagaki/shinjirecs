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
  class RecordThread < Thread
    attr_reader :reservation,:stop_time
  end

  belongs_to :channel
  belongs_to :program_series

  minimum :counter, 1

  enum state: { waiting: 0, preparing: 1, recording: 2, success: 3, failed: -1, canceled: -2 }

  validate do |rec|
    rec.errors[:stop_time] << "inconsistent stop_time" if not rec.start_time < rec.stop_time
    rec.errors[:stop_time] << "this reservation will not record" if not Time.now < rec.stop_time
    rec.errors[:start_time] << "will not use tuner,because count of overrapped reservations is over than tuner's count" if not rec.will_recordable?

    if (self.recording? or self.preparing?) and self.start_time_changed? then
      rec.errors[:start_time] << "cannot change start_time when preparing and recording"
    end

    # if self.event_id then
    #   if self.new_record? then
    #     rec.errors[:event_id] << "invalid event_id" if self.guess_epg_programs.count < 1
    #   else
    #     rec.errors[:event_id] << "invalid event_id" if self.guess_epg_programs.count < 1 and self.event_id_changed?
    #   end
    # end
  end

  scope :will_use_tuner, ->(){ where(state: [:waiting, :preparing, :recording]) }

  scope :future_stop_time, -> (){
    where(["stop_time > ?",Time.now])
  }

  def future_stop_time?
    self.stop_time > Time.now
  end

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
    if self.recording? then
      self.command_pid = 0
    end
  end

  def self.video_dir
    Rails.application.config.video_path
  end

  scope :overlapped, ->(st,ed) {
    where(["not (start_time > ? or stop_time < ?)", ed, st])
  }

  def self.select_overlapped_proxy(st,ed,ctype,exclude_ids=[])
    proxy = self.in_ctype(ctype).overlapped(st,ed)
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
    max = self.max_overlapped_count(allat,rsvs)
    max <= count
  end

  def filepath_fix
    (self.class.video_dir + "./" + self.filename).to_s
  end

  def partial_filepath(n,base=nil)
    (base || self.filepath_fix) + ".part#{n}"
  end

  def filepath(add_suffix_if_exists=false)
    rtn = self.filepath_fix
    if add_suffix_if_exists then
      partnum = 0
      newrtn = rtn + ""
      while File.exists? newrtn
        newrtn = self.partial_filepath(partnum,rtn)
        partnum += 1
      end
      rtn = newrtn
    end
    rtn
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

  def tuner_count
    System.instance.tuner_count(self.channel.ctype)
  end

  def rest_tuner_count
    System.instance.rest_tuner_count(self.channel.ctype)
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

  @@recordings = []

  scope :will_start_in, ->(sec = 10){
    now ||= Time.now
    # where{ start_time.gt(now) & start_time.lt(now+sec) }
    where("start_time > ?", now).where("start_time < ?", now+sec)
  }

  def will_start_in?(sec)
    now = Time.now
    self.start_time > now && self.start_time < now + sec
  end

  scope :now_in_recording_time, ->() {
    now = Time.now
    where("start_time <= ?", now ).where("stop_time >= ?", now)
  }

  def now_in_recording_time?
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

  def self.mk_cmd_by_result(cmdres)
    cmdpath = nil
    case cmdres
    when GetCommandPathResult::GetSuccess
      cmdpath = cmdres.path
    # when GetCommandPathResult::NotExecutable
    # when GetCommandPathResult::NotFound
    end
    cmdpath
  end

  def mk_recording_cmd
    return nil if not (cmdpath = self.class.mk_cmd_by_result Command.recording_cmd)
    fpath = self.filepath(true)
    ["#{cmdpath} #{self.channel.number} #{self.duration_sec} " + fpath , fpath]
  end

  def self.log_splitter
    "7186cc6a2ef74523c310"
  end

  def command_pid
    pid = self.attributes["command_pid"]
    (pid and pid < 1) ? nil : pid
  end

  # @return false => needless or fail to send term message
  # @return true  => success to send term message
  def stop_recording
    self.reload
    if not self.recording?
      return false
    end

    if pid = self.command_pid then
      return false
    end

    Reservation.transaction do
      begin
        self.state.cancel!
        Process.kill Signal.list["TERM"], pid
      rescue ArgumentError => e
        # process of pid was not found because maybe the process was already terminated
        return true
      rescue => e
        raise ActiveRecord::Rollback.new e.message
      end
    end
    false
  end

  def extend_recording_if_need
    return nil if not (cmdpath = self.class.mk_cmd_by_result Command.extend_recording_time_cmd)
    if not self.recording?
      return false
    end

    if pid = self.command_pid then
      return false
    end
    "#{cmdpath} #{pid} #{sec}"
  end

  def self.preparing_margin
    10.0
  end

  def self.start_margin
    3.0
  end

  def self.end_margin
    self.start_margin + 5.0
  end

  def in_preparing_time?
    span = self.start_time - Time.now
    0 <= span && span <= self.class.preparing_margin
  end

  def start_recording
    if @recth then
      return @recth
    end

    if not self.future_stop_time? then
      return false
    end

    @recth = RecordThread.start(self) do |rsv|
      th = Thread.current
      th.reservation = rsv
      th.stop_time = rsv.stop_time
      while (sleepsec = (restsec = rsv.start_time - Time.now) - rsv.class.preparing_margin) > 0
        sleep sleepsec

        # reload data because there is a possibility that data was changed while this thread slept
        rsv.reload
      end
      rsv.state.preparing!

      cmd,resfpath = rsv.mk_recording_cmd
      if not cmd then
        rsv.state.canceled!
        next # finish
      end
      sleep(rsv.start_time - Time.now - rsv.class.start_margin)

      reslog = ""
      Open3.popen3(cmd) do |i,o,e,w|
        pid = o.gets.to_i # first line of stdout is pid of recording process
        rsv.update(command_pid: pid, command_str: cmd, state: "recording")
        while line = e.gets # stderr is used for message
          puts line
          reslog += line
        end
      end

      reslog = "\n#{rsv.class.log_splitter}\n" + Time.now.to_s + "\n" + resfpath + "\n" + reslog
      rsv.reload

      log = rsv.log
      errlog = rsv.error_log
      state = rsv.state

      if rsv.state.canceled? then
        errlog += reslog # canceled by other thread
      else
        if File.exists? resfpath then
          state = "success"
          log += reslog
        else
          state = "failed"
          errlog += reslog
        end
      end
      rsv.state = state
      rsv.log = log
      rsv.error_log = errlog
      rsv.save!
    end
  end

  after_create_commit :after_create_commit_impl

  def after_create_commit_impl
    if self.now_in_recording_time? then
      self.class.check_preparing
    else
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
    ( self.stop_time - self.start_time - self.class.end_margin).to_i
  end

  def duration_min
    ( self.duration_sec / 60 ).to_i
  end

  def duration
    self.duration_sec
  end

  def event_id
    v = self.attributes["event_id"]
    (v < 1) ? nil : v
  end

  def guess_epg_programs
    day = 24*3600
    proxy = EpgProgram
      .where("start_time > ? and stop_time < ?", self.start_time - day, self.stop_time + day)
      .where(channel_id: self.channel_id)
      .order(desc: :start_time)
    if self.event_id then
      proxy = proxy.where(event_id: self.event_id)
    end
    proxy.to_a
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

