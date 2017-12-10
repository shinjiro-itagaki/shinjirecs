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
  require "open3"

  class RecordThread < Thread
    attr_accessor :reservation,:stop_time,:pid
  end

  belongs_to :channel
  belongs_to :program_series

  minimum :counter, 1

  enum state: { waiting: 0, preparing: 1, recording: 2, success: 3, failed: -1, canceled: -2 }

  validate do |rec|
    rec.errors[:stop_time] << "inconsistent stop_time" if not rec.start_time < rec.stop_time
    rec.errors[:stop_time] << "this reservation will not record" if not Time.now < rec.stop_time
    rec.errors[:start_time] << "will not use tuner,because count of overrapped reservations is over than tuner's count" if not rec.will_recordable?

    # check whether other reservation will be impossible to record, or not
    if self.will_record_state?
      if self.new_record? then
        msg = "invalid insert , because other reservation will be imppossible to record by this insert."
        # if (overlapped_count = self.class.select_overlapped_proxy(self.start_time, self.stop_time, ctype).count) <
      else
        if self.start_time_changed? or self.stop_time_changed? then
          ctype = self.channel.ctype
          exclude_ids=[self.id]
          old_overlapped_count = self.class.select_overlapped_proxy(self.start_time_was,self.stop_time_was,ctype,exclude_ids).count
          new_overlapped_count = self.class.select_overlapped_proxy(self.start_time,    self.stop_time,    ctype,exclude_ids).count
          puts "old_overlapped_count = #{old_overlapped_count}"
          puts "new_overlapped_count = #{new_overlapped_count}"
          if new_overlapped_count > old_overlapped_count
            msg = "invalid change %s , because other reservation will be imppossible to record by this change."
            rec.errors[:start_time] << msg % ["start_time"] if self.start_time_changed?
            rec.errors[:stop_time]  << msg % ["stop_time"]  if self.stop_time_changed?
          end
        end
      end
    end

    # if self.event_id then
    #   if self.new_record? then
    #     rec.errors[:event_id] << "invalid event_id" if self.guess_epg_programs.count < 1
    #   else
    #     rec.errors[:event_id] << "invalid event_id" if self.guess_epg_programs.count < 1 and self.event_id_changed?
    #   end
    # end
  end

  scope :use_tuner_state, ->(){ where(state: [:waiting, :preparing, :recording]) }

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
    self.use_tuner_state.future_stop_time
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

  def self.default_all_proxy
    super.order(start_time: :desc)
  end

  def filesize
    path = self.filepath_fix
    if (not self.waiting?) and (not path.empty?) and (File.exists? path) then
      File.size(path)
    else
      nil
    end
  end

  def self.video_dir
    Rails.application.config.video_path
  end

  scope :overlapped, ->(st,ed) {
    where(["not (start_time > ? or stop_time < ?)", ed, st]).use_tuner_state
  }

  def self.select_overlapped_proxy(st,ed,ctype,exclude_ids=[])
    proxy = self.in_ctype(ctype).overlapped(st,ed)
    if not exclude_ids.empty? then
      proxy = proxy.where.not(id: exclude_ids)
    end
    proxy
  end

  def self.full?(st,ed,ctype)
    !(select_overlapped_proxy(st,ed,ctype).count < System.instance.tuner_count(ctype))
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

  # if filename is empty, return ""
  def filepath_fix
    if (name = self.filename).empty? then
      ""
    else
      (self.class.video_dir + "./" + name).to_s
    end
  end

  def file_access_path
    (self.class.video_access_dir + "./" + self.filename).to_s
  end

  def partial_filepath(n,base=nil)
    (base || self.filepath_fix) + ".part#{n}"
  end

  # if filename is empty, return ""
  def filepath(add_suffix_if_exists=false)
    rtn = self.filepath_fix
    return "" if rtn.empty?

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
    self.class.select_overlapped_proxy(self.start_time, self.stop_time, self.channel.ctype, ex_ids)
  end

  def overlapped
    self.select_overlapped_proxy.all
  end

  def will_use_tuner_count()
    self.select_overlapped_proxy.count
  end

  def will_record_state?
    self.waiting? or self.recording? or self.preparing?
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
    self.select_overlapped_proxy(exclude_self = true)
      .to_a.append(self)
      .sort_by{|a,b| self.class.compare(a,b) }
      .map(&:__id__)
      .take(self.tuner_count)
      .include?(self.__id__)
  end

  def will_recordable_if_persisted?
    if self.will_record_state? then
      self.select_overlapped_proxy(exclude_self = false)
        .limit(self.tuner_count)
        .to_a
        .sort_by{|a,b| self.class.compare(a,b) }
        .map(&:id)
        .include?(self.id)
    else
      self.will_recordable_if_new?
    end
  end

  scope :now_in_recording_time, ->() {
    now = Time.now
    where("start_time <= ?", now ).where("stop_time >= ?", now)
  }

  scope :staging, ->() {
    now = Time.now
    use_tuner_state.where("stop_time > ? and start_time < ?", now, now + self.preparing_margin).order(start_time: :asc, id: :asc)
  }

  def self.random(chnumber=nil)
    channel = nil

    if chnumber then
      channel = Channel.where(number: chnumber).first
    else
      chn = (Time.now.to_i / 60).to_i
      chs = Channel.enables.to_a
      chs = Channel.exist.to_a if chs.count < 1
      chs = Channel.all.to_a if chs.count < 1
      channel = chs[chn % chs.count] if chs.count > 0
    end

    return nil if not channel

    st = Time.now + 5
    ed = Time.now + 35
    duration_sec = ed - st
    wday_mask = Weekdays.to_mask(st.wday)
    ps = ProgramSeries.find_or_create_by(channel.id, st, duration_sec,  wday_mask)
    self.new(program_series_id: ps.id, start_time: st, stop_time: ed, channel_id: channel.id, title: ps.label, desc: ps.desc)
  end

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

  def self.compare(a,b)
    if a.kind_of? self and b.kind_of? self
      (a.start_time <=> b.start_time).nonzero? || (a.id <=> b.id)
    else
      nil
    end
  end

  @@do_check_staging_now = false
  @@staging = {}

  def self.check_staging
    if @@do_check_staging_now then
      return false
    end
    @@do_check_staging_now = true

    @@staging = self.staging.to_a.inject(Hash.new){|h,e| h[e.id]=e;h}.merge(@@staging)
    @@staging.values.sort_by{|a,b|
      self.compare(a,b)
    }.each do |r|
      if r.will_record_state? then
        r.start_recording_thread_if_not_started
      else
        @@staging.delete(r.id)
      end
    end
  ensure
    rtn = self.next_check_staging_time
    @@do_check_staging_now = false
    return rtn # dont forget 'return'
  end

  def self.staging_keys
    @@staging.keys
  end

  def self.stagings
    @@staging.values
  end

  def self.next_check_staging_time
    self.will_record.where.not(id: self.staging_keys).pluck(:start_time).min
  end

  # @return :: [result : Bool, cmdpath : String, message : String]
  def self.mk_cmd_by_result(cmdres)
    cmdfpath = cmdres.path
    res = true
    msg = ""
    case cmdres
    when Command::GetCommandPathResult::GetSuccess
    when Command::GetCommandPathResult::NotExecutable
      res = false
      msg = "'#{cmdfpath}' is not executable"
    when Command::GetCommandPathResult::NotFound
      res = false
      msg = "'#{cmdfpath}' is not found"
    end
    [res, cmdfpath, msg]
  end

  # @return :: [result : Bool, cmd : String, cmdfpath : String, message : String]
  def mk_recording_cmd
    res,cmdfpath,msg = self.class.mk_cmd_by_result Command.recording_cmd
    outputfpath = self.filepath(true)
    [res, "#{cmdfpath} #{self.channel.number} #{self.duration_sec} #{outputfpath}", cmdfpath, outputfpath, msg]
  end

  def self.mk_encoding_cmd(inn,opts="",out=nil)
    out ||= inn + ".mpeg"
    res,cmdfpath,msg = self.mk_cmd_by_result Command.encoding_cmd
    [res, "#{cmdfpath} #{inn} \"#{opts}\" #{out}", cmdfpath, out, msg]
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

    if not (pid = self.command_pid) then
      return false
    end

    Reservation.transaction do
      begin
        self.cancel!
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

  def extend_recording_state_waiting(sec)
    if not self.waiting? then
      puts "state is not waiting"
      return false
    end
    self.stop_time += sec
    self.save!
  end

  def extend_recording_state_recording(sec)
    if not self.recording? then
      puts "state is not recording"
      return false
    end

    if not (pid = self.command_pid) then
      puts "process '#{pid}' command not found"
      return false
    end

    cmdpath = self.class.mk_cmd_by_result Command.extend_recording_time_cmd
    return false if not cmdpath

    begin
      self.class.transaction do
        self.stop_time += sec
        self.save!
        cmd = "#{cmdpath} #{pid} #{sec}" #hoge
        res = nil
        Open3.popen3(cmd) do |i,o,e,w|
          o.read
          res = w
        end

        if res and res.success? then
        else
          raise ActiveRecord::Rollback.new("error: '#{cmd}'")
        end
      end
    rescue => e
      puts e.message
      return false
    end
  end

  def extend_recording(sec)
    if self.waiting?
      return extend_recording_state_waiting(sec)
    end

    if self.recording?
      return extend_recording_state_recording(sec)
    end

    true
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

  # not implemented
  def concat_partials
  end

  # not implemented
  def self.retry_failures
  end

  # not implemented
  def retry_if_failed
  end

  scope :be_consistent, ->() {
    where(state: "recording").where("start_time < ?", Time.now - 60)
  }

  def correct_if_be_consistent
    if self.recording? and self.stop_time < (Time.now - 60) then
      return self.stop_recording
    end
    false
  end

  def self.encoding(inn,options="",out=nil)
    # inn ||= "pipe:0"
    res,cmd,cmdfpath,outputfpath,msg = mk_encoding_cmd(inn,options,out || inn+".mpeg")
    if not res then
      puts "command not found"
      return nil
    end

    if not File.exists?(inn) then
      return nil
    end

    Open3.popen3(cmd) do |i,o,e,w|
      puts "start #{cmd}"
      puts e.read
      puts o.read
    end
  end

  def encoded?
    File.exists? enc_filepath
  end

  # return "" if filepath is empty
  def enc_filepath
    if (path = self.filepath).empty? then
      ""
    else
      path + ".mpeg"
    end
  end

  def enc_filesize
    if (not (path = self.enc_filepath).empty?) and File.exists?(path) then
      File.size path
    else
      nil
    end
  end

  def encoding(force=true)
    if File.exists?(path = self.enc_filepath) and not force then
      true
    else
      self.class.encoding(self.filepath,"",path)
    end
  end

  def self.run_record_thread_impl(rsv)
    if not rsv.will_record_state? then
      puts "this is not recordable state"
      return false
    end

    puts "start new reservation thread"
    puts "start_time=#{rsv.start_time}, stop_time=#{rsv.stop_time}, ch=#{rsv.channel.number}"
    th = Thread.current
    if th.kind_of? RecordThread
      th.reservation = rsv
      th.stop_time = rsv.stop_time
    end

    puts "before sleep"
    while true
      sleepsec = (wakeup_time = rsv.start_time - rsv.class.preparing_margin) - Time.now
      if sleepsec > 0 then
        puts "this reservation thread wakeup after #{sleepsec} (at #{wakeup_time})"
        sleep sleepsec

        # reload data because there is a possibility that data was changed while this thread slept
        rsv.reload
      else
        puts "no need to sleep"
        break
      end
    end

    if rsv.waiting?
      rsv.preparing!
      puts "set preparing ..."
    end

    res,cmd,cmdfpath,outputfpath,msg = rsv.mk_recording_cmd
    if not res then
      rsv.canceled!
      puts "[ERROR] #{msg}"
      return false
    end

    sleepsec = rsv.start_time - Time.now - rsv.class.start_margin
    sleep(sleepsec) if sleepsec > 0.0

    reslog = ""
    puts "start recording ..."
    now_recording = true

    Open3.popen3(cmd) do |i,o,e,w|
      pid = o.gets.to_i # first line of stdout is pid of recording process
      puts "pid=#{pid}"
      rsv.update(command_pid: pid, command_str: cmd, state: "recording")
      while line = e.gets # stderr is used for message
        puts line
        reslog += line
      end
    end
    puts "finish recording ..."
    now_recording = false

    reslog = "\n#{rsv.class.log_splitter}\n" + Time.now.to_s + "\n" + outputfpath + "\n" + reslog
    rsv.reload

    log = rsv.log.to_s
    errlog = rsv.error_log.to_s
    state = rsv.state

    if rsv.canceled? then
      errlog += "\n" + reslog # canceled by other thread
    else
      if File.exists? outputfpath then
        state = "success"
        log += "\n" + reslog
      else
        state = "failed"
        errlog += "\n" + reslog
      end
    end
    rsv.state = state
    rsv.log = log
    rsv.error_log = errlog
    rsv.save!
  end

  def record_thread_finished!
    if @recth then
      @recth.exit
    end
    @recth = nil
    @record_thread_finished=true
  end

  def record_thread_finished?
    @record_thread_finished
  end

  def start_recording_thread_if_not_started
    if not self.future_stop_time? then
      return false
    end

    @recth = nil if self.record_thread_finished?
    @recth ||= RecordThread.start(self) do |rsv|
      begin
        ActiveRecord::Base.connection_pool.with_connection do
          self.class.run_record_thread_impl(rsv)
          rsv.record_thread_finished!
        end
      rescue => e
        puts e
      end
    end
  end

  after_create_commit :after_create_commit_impl

  def after_create_commit_impl
  end

  after_commit :after_commit_impl

  def after_commit_impl
    Rails.application.wakeup_or_start_observer_thread
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
      .order(start_time: :desc)
    if self.event_id then
      proxy = proxy.where(event_id: self.event_id)
    end
    proxy.to_a
  end

  def as_json(options = nil)
    { filepath: self.filepath,
      start_time_str: self.start_time.to_s,
      stop_time_str: self.stop_time.to_s,
      filesize: self.filesize,
      enc_filepath: self.encoded? ? self.enc_filepath : nil,
      enc_filesize: self.encoded? ? self.enc_filesize : nil
    }.merge(super(options))
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

