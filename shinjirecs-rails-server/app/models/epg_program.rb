# t.timestamp  "start_time"           , null: false
# t.timestamp  "stop_time"            , null: false
# t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :restrict, on_update: :cascade}
# t.string     "title"                , null: false
# t.text       "desc"                 , null: false
# t.integer    "program_category_id"  , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
class EpgProgram < ApplicationRecord
  belongs_to :channel
  has_and_belongs_to_many :epg_program_categories,        foreign_key: "program_id", association_foreign_key: "category_id", join_table: "epg_program_category_maps"
  has_and_belongs_to_many :epg_program_medium_categories, foreign_key: "program_id", association_foreign_key: "category_id", join_table: "epg_program_medium_category_maps"
  has_and_belongs_to_many :video_types,                   foreign_key: "program_id", association_foreign_key: "video_type_id", join_table: "epg_program_video_type_maps"
  has_and_belongs_to_many :audio_types,                   foreign_key: "program_id", association_foreign_key: "audio_type_id", join_table: "epg_program_audio_type_maps"
  has_and_belongs_to_many :attachinfos,                   foreign_key: "program_id", association_foreign_key: "attachinfo_id", join_table: "epg_program_attachinfo_maps"

  validate do |rec|
    rec.errors[:stop_time] << "inconsistent stop_time, start_time=#{rec.start_time}, stop_time=#{rec.stop_time} , #{rec}" if not rec.start_time < rec.stop_time
    rec.errors[:event_id]  << "event_id '#{rec.event_id}' is not unique around stop_time '#{rec.stop_time}'"  if rec.new_record? and not rec.unique_event_id?
  end

  default_scope { order(start_time: :desc) }

  before_save do
    self.desc ||= ""
  end

  def self.output_reflections?
    true
  end

  def self.output_reflections(ins=nil)
    res = {}
    self.reflections.keys.each do |key|
      v = ins.send key
      if v.kind_of? ActiveRecord::Relation then
        res[key] = v.to_a.map(&:id) # v.to_a.map{|x| x.orig_as_json options }
      else
        #v = v.orig_as_json(options)
      end
      # res[key] = v
    end
    res
  end

  def self.default_all_proxy
    super.default
  end

  def self.import_epg(json, chnumber=nil)
    json.each do |d|
      ch = Channel.find_or_import_channel_by_json(d, chnumber)
      # skip import programs on this channel
      # because it is impossible to identify the channel
      next if not ch
      ch.exist = true
      return if not Thread.current.status
      ch.save!

      return if not Thread.current.status

      # skip if chnumber is decleared and detected channel is not match it
      next if chnumber and ch.number != chnumber
      (d["programs"] || []).each do |p|
        self.find_or_import_program_by_json(ch,p)
        return if not Thread.current.status
      end
    end
  end

  def unique_event_id?
    self.class.find_or_initialize_by_event_id(self.event_id, self.channel_id, self.stop_time).new_record?
  end

  def self.find_or_initialize_by_event_id(event_id, ch_id, stop_time)
    rtn = self.where(event_id: event_id, channel_id: ch_id).where(["stop_time > ? and stop_time <= ?", stop_time - 1.day , stop_time + 1.day  ]).first
    rtn || self.new(event_id: event_id, channel_id: ch_id, stop_time: stop_time)
  end

  def self.find_or_import_program_by_json(ch,p)
    ch_id = (ch.kind_of?(Channel) ? ch.id : ch)
    event_id = p["event_id"]
    stop_time = Time.at(p["end"].to_i / 10000)
    prec = self.find_or_initialize_by_event_id(event_id, ch_id, stop_time)
    prec.stop_time  = stop_time
    prec.start_time = Time.at(p["start"].to_i / 10000)
    prec.title      = p["title"]
    prec.freeCA     = p["freeCA"]
    desc = p["detail"].to_s + "\n"
    (p["extdetail"] || []).each do |item|
      desc += item["item_description"].to_s + "\n"
      desc += item["item"].to_s + "\n"
    end
    prec.desc = desc

    cids = prec.epg_program_categories.pluck(:id)
    cmids = prec.epg_program_medium_categories.pluck(:id)

    lcats = []
    mcats = []

    (p["category"] || []).each do |cat|
      return if not Thread.current.status
      if l_cat = cat["large"] then
        lcat = EpgProgramCategory.find_or_create_by!(label_ja: l_cat["ja_JP"], label_en: l_cat["en"])
        if not cids.include?(lcat.id) then
          lcats << lcat
        end
      end

      if lcat && lcat.id && m_cat = cat["middle"] then
        mcat = EpgProgramMediumCategory.find_or_create_by!(parent_id: lcat.id, label_ja: m_cat["ja_JP"], label_en: m_cat["en"])
        if not cmids.include?(mcat.id) then
          mcats << mcat
        end
      end
    end

    prec.epg_program_categories        << lcats.uniq(&:id)
    prec.epg_program_medium_categories << mcats.uniq(&:id)

    if attachinfos = p["attachinfo"] then
      ids = prec.attachinfos.pluck(:id)
      prec.attachinfos << [attachinfos].flatten.map{|str| Attachinfo.find_or_create_by(desc: str) }.uniq(&:id).delete_if{|x| ids.include? x.id }
    end

    if videos = p["video"] then
      ids = prec.video_types.pluck(:id)
      prec.video_types << [videos].flatten.map{|video| VideoType.find_or_create_by(resolution: video["resolution"], aspect: video["aspect"]) }.uniq(&:id).delete_if{|x| ids.include? x.id }
    end

    if audios = p["audio"] then
      ids = prec.audio_types.pluck(:id)
      prec.audio_types << [audios].flatten.map{|audio| AudioType.find_or_create_by(typ: audio["type"], langcode: audio["langcode"], extdesc: audio["extdesc"]) }.uniq(&:id).delete_if{|x| ids.include? x.id }
    end
    return if not Thread.current.status

    prec.save!
    prec
  end

  def overlap?(start,endt)
    return false if start.kind_of? Time
    return false if endt.kind_of?  Time
    if not start < endt then
      start,endt = endt,start
    end
    st = self.start_time
    et = self.stop_time

    ###########################
    # xy1 not overlap 1
    ###########################
    #  start       (*) endt
    #    |------------|
    #                    |------------|
    #                  (*) st        et
    xy1 = endt < st

    ###########################
    # xy2 not overlap 2
    ###########################
    #               (*) start      endt
    #                  |------------|
    #   |------------|
    #  st         (*) et
    ###########################
    xy2 = et < start

    not_overlap = xy1 || xy2

    return !not_overlap
  end

  def include?(time)
    self.start_time <= time and time <= self.stop_time
  end

  def duration_sec
    ( self.stop_time - self.start_time ).to_i
  end

  def duration_min
    ( self.duration_sec / 60 ).to_i
  end

  def find_or_create_program_series
    ProgramSeries.find_or_create_by_epg_program(self)
  end

  def new_reservation(params={})
    if self.stop_time <= Time.now
      return nil
    end

    series = self.find_or_create_program_series

    wd = nil
    if x = params["weekdays"] then
      wd = x.to_i
    end

    if params["everyweek"] then
      wd = Weekdays.to_mask(self.start_time.wday)
    end

    if wd then
      series.update(weekdays: wd)
    end
    
    Reservation.new(start_time: self.start_time,
                    stop_time: self.stop_time,
                    channel_id: self.channel_id,
                    program_series_id: series.id,
                    title: self.title,
                    desc: self.desc,
                    event_id: self.event_id,
                    counter: series.next_episode_number)
  end

  def self.epgdump(channel_numbers_or_filepaths=nil, sec=240)
    channel_numbers_or_filepaths ||= Channel.default_all_numbers

    if not channel_numbers_or_filepaths.kind_of? Array then
      channel_numbers_or_filepaths = [channel_numbers_or_filepaths]
    end

    cmdfile = Command.epgdump_cmd
    cmdfilepath = cmdfile.path
    case cmdfile
    when Command::GetCommandPathResult::GetSuccess
    when Command::GetCommandPathResult::NotFound
      puts cmdfilepath + " is not found."
      return
    when Command::GetCommandPathResult::NotExecutable
      puts cmdfilepath + " is not executable."
      return
    end

    res = {}
    channel_numbers_or_filepaths.map do |chnum_or_fpath|
      #  in(ch) out sec
      chnumber = File.exists?(File.expand_path(chnum_or_fpath.to_s)) ? nil : chnum_or_fpath
      cmd = "#{cmdfilepath} #{chnum_or_fpath} - #{sec}"
      puts cmd
      begin
        json = JSON.parse(`#{cmd}`)
      rescue
        # json parse error
        puts e
      end
      puts json.length
      return if not Thread.current.status
      self.import_epg(json,chnumber)
      res[chnum_or_fpath]=json
    end
    res
  end

  def self.default_search_start_time
    (Time.now - 2.weeks.seconds).beginning_of_day
  end

  scope :default, ->(start_time=nil) {
    st = Time.at(start_time || EpgProgram.default_search_start_time.to_i)
    where("start_time > ?", st).order(start_time: :desc)
  }

  def self.search(params)
    proxy = nil
    if not (w = params["word"].to_s.strip).empty? then
      proxy = where("\`title\` LIKE ?","%#{w}%").or(self.where("\`desc\` LIKE ? ","%#{w}%"))
    end
    proxy = (proxy || self).default(params["start_time"])
    if (inn = params["start_in"].to_i) and inn > 0 then
      st = Time.now
      ed = st + inn
      proxy = self.where("start_time >= ?", st).where("start_time <= ?", ed)
    end
    proxy.joins(:channel).merge(Channel.enables)
  end

  def as_json(options = nil)
    {start_time_str: self.start_time.to_s, stop_time_str: self.stop_time.to_s }.merge(super(options))
  end
end
