# t.timestamp  "start_time"           , null: false
# t.timestamp  "stop_time"            , null: false
# t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :restrict, on_update: :cascade}
# t.string     "title"                , null: false
# t.text       "desc"                 , null: false
# t.integer    "program_category_id"  , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
class EpgProgram < ApplicationRecord
  belongs_to :channel
  has_and_belongs_to_many :epg_program_categories,        association_foreign_key: "category_id", join_table: "epg_program_category_maps"
  has_and_belongs_to_many :epg_program_medium_categories, association_foreign_key: "category_id", join_table: "epg_program_medium_category_maps"
  has_and_belongs_to_many :video_types,                   association_foreign_key: "video_type_id", join_table: "epg_program_video_type_maps"
  has_and_belongs_to_many :audio_types,                   association_foreign_key: "audio_type_id", join_table: "epg_program_audio_type_maps"
  has_and_belongs_to_many :attachinfos,                   association_foreign_key: "attachinfo_id", join_table: "epg_program_attachinfo_maps"

  validate do |rec|
    rec.errors[:stop_time] << "inconsistent stop_time" if not rec.start_time < rec.stop_time
  end

  def overlap?(start,endt)
    return false if start.kind_of? Time
    return false if endt.kind_of?  Time
    if not start < endt then
      start,endt = endt,start
    end
    st = self.starttime
    et = self.stoptime

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
    self.starttime <= time and time <= self.stoptime
  end

  def length_second
    ( self.stoptime - self.starttime ).to_i
  end

  def length_min
    ( self.length_second / 60 ).to_i
  end

  # guess ProgramTitle by Weekday and Time and etc
  def guess_title
  end

  def new_reservation
    r = Reservations.new
    r.start_time = self.start_time
    r.duration   = self.length_second
    r.channel_id = self.channel_id
    # program_title_id : default 0
    r.title = self.title
    r.desc = self.desc
    r.event_id = self.event_id
    # t.integer    "counter"             , null: false , default: 0
    # t.integer    "state"               , null: false , default: 0, limit: 1

    @reservation ||= Reservations.find(r.id)[0]
  end
end
