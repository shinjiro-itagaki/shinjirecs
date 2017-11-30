class Channel < ApplicationRecord
  enum ctype: {gr: 'gr',bs: 'bs'}
  belongs_to :area
  has_many :reservations
  has_many :program_series
  has_many :epg_programs

  before_save :auto_set_values

  scope :enables, ->(){ where(enable: true) }
  scope :exist,   ->(){ where(exist:  true) }
  scope :scaned,  ->(){ where(scaned: true) }

  def self.minimum_number() 1 end

  validates :display_name, presence: false
  validates_uniqueness_of :service_id, scope: :area_id

  def self.default_all_numbers
    # self.default_all_gr_numbers + self.default_all_bs_numbers
    self.exist.pluck(:number)
  end

  def self.default_all_gr_numbers
    (13..62).to_a
  end

  def self.default_all_bs_numbers
    [
      "BS1_0" ,"BS1_1",
      "BS3_0" ,"BS3_1",
      "BS5_0" ,"BS5_1",
      "BS7_0" ,"BS7_1" ,"BS7_2",
      "BS9_0" ,"BS9_1" ,"BS9_2",
      "BS11_0","BS11_1","BS11_2",
      "BS13_0","BS13_1",
      "BS15_0","BS15_1",
      "BS17_0","BS17_1",
      "BS19_0","BS19_1","BS19_2",
      "BS21_0","BS21_1","BS21_2",
      "BS23_0","BS23_1","BS23_2",
    ]
  end

  def self.params_info
    s = super
    s["number"].merge "minimum" => self.minimum_number
    s
  end

  def self.find_or_import_channel_by_json(d,dflt_chnumber=nil)
    chnumber = dflt_chnumber
    # channel
    service_id = d["service_id"]
    name       = d["name"]
    transport_stream_id = d["transport_stream_id"]

    sat_tp   = nil
    sat_slot = nil
    if satinfo = d["satelliteinfo"] then
      sat_tp   = satinfo["TP"]
      sat_slot = satinfo["SLOT"]
    end

    if sat_tp && sat_slot then
      chnumber = "#{sat_tp}_#{sat_slot}"
    end

    ch = System.instance.find_or_initialize_channel_by_sid(service_id)
    if ch.new_record? then
      if not chnumber then
        # skip import channel
        # because it is impossible to identify the channel
        return nil
      end
      ch = System.instance.find_or_initialize_channel_by_number(chnumber)
      if ch.new_record? then
      else
        if ch.enable_service_id? then
          # initialize new channel which has same number
          ch = System.instance.new_channel
          ch.number = chnumber
        end
      end
      ch.service_id = service_id
      ch.display_name=name
    end

    if not ch.display_name_locked? then
      ch.display_name=name
    end

    ch.save!
    ch
  end

  def enable_service_id?
    self.service_id > 0
  end

  def self.scan_timeout
    5
  end

  def self.scan(gr=nil,bs=nil,timeout=nil,&block)
    gr ||= self.default_all_gr_numbers
    bs ||= self.default_all_bs_numbers
    timeout_sec = timeout.to_s.to_i
    timeout_sec = self.scan_timeout if timeout_sec < 1

    area_id = System.instance.area_id
    cmdfile = Command.scan_channel_cmd
    cmdfilepath = cmdfile.path

    case cmdfile
    when Command::GetCommandPathResult::GetSuccess
    when Command::GetCommandPathResult::NotFound
      puts cmdfilepath + " is not found."
      render_data nil
      return
    when Command::GetCommandPathResult::NotExecutable
      puts cmdfilepath + " is not executable."
      render_data nil
      return
    end
    if block and block.call then
      return true
    end
    {"gr" => gr,"bs" => bs}.each do |type,charr|
      charr = charr.map {|ch|
        c = Channel.find_or_initialize_by(number: ch, area_id: area_id, ctype: type)
        if c.new_record?
          c.scaned = false
          c.save!
        end
        c
      }

      charr.each do |ch|
        tempfile = Tempfile.new('for-scan')
        tempfilepath = tempfile.path
        cmd = "#{cmdfilepath} #{ch.number} #{timeout_sec} #{tempfilepath}"
        tempfile.unlink

        puts cmd
        res = false
        scaned = true
        # io = IO.popen(cmd, "r")
        if block and block.call then
          return true
        end
        pid = spawn(cmd, pgroup: Process.pid)  # io.pid
        puts "command pid=#{pid}"
        watch_thread = Process.detach(pid)
        term = false
        if block and block.call then
          term = true
          Process.kill Signal.list["TERM"], pid
        end
        watch_thread.join

        if block and block.call then
          return true
        end

        if term then
          return true
        end

        if File.exists?(tempfilepath) then
          puts "command success  ......... channel '#{ch.number}' was DETECTED!! "
          ch.exist = true
        else
          puts "command failed. Channel '#{ch.number}' was not found."
          ch.exist = false
        end
        ch.scaned = scaned
        ch.save!
      end
    end
  end

  private
  def set_default
    self.ctype        ||= 'gr'
    # self.display_name ||= "No Name " + Time.now.to_s(:number)
  end

  def auto_set_values
    if (self.display_name || "").empty?
      self.display_name = self.number
    end
    self.service_id ||= (Channel.where(area_id: self.area_id).order(service_id: :asc).pluck(:service_id).first || 0) - 1
  end
end
