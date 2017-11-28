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
    rec.errors[:stop_time] << "inconsistent stop_time" if not rec.start_time < rec.stop_time
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
    super.where(["start_time > ?", Time.now - 1.week])
  end

  def self.import_epg(json, chnumber=nil)
    json.each do |d|
      ch = Channel.find_or_import_channel_by_json(d, chnumber)
      # skip import programs on this channel
      # because it is impossible to identify the channel
      next if not ch
      ch.exist = true
      ch.save!

      # skip if chnumber is decleared and detected channel is not match it
      next if chnumber and ch.number != chnumber
      (d["programs"] || []).each do |p|
        self.find_or_import_program_by_json(ch,p)
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
    prec.start_time = Time.at(p["start"].to_i / 10000)
    prec.title      = p["title"]
    prec.freeCA     = p["freeCA"]
    prec.desc       = p["detail"].to_s + "\n" + (p["extdetail"] || []).map(&:to_s).join("\n")

    cids = prec.epg_program_categories.pluck(:id)
    cmids = prec.epg_program_medium_categories.pluck(:id)

    lcats = []
    mcats = []

    (p["category"] || []).each do |cat|
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

  def new_reservation
    if self.stop_time <= Time.now
      return nil
    end

    series = ProgramSeries.find_or_create_by_epg_program(self)
    series.new_next_reservation(self)
  end
end
