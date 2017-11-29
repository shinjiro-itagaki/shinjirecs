      # t.integer    "start_at"             , null: false
      # t.integer    "duration"             , null: false
      # t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :restrict, on_update: :cascade}
      # t.string     "name"                 , null: false
      # t.boolean    "repeat"               , null: false , default: false
      # t.text       "desc"                 , null: false
      # t.integer    "next_episode_number"  , null: false , default: 1
      # t.integer    "last_episode_number"  , null: false , default: 0
      # t.integer    "weekdays"             , null: false , default: 0, limit: 1 # byte
      # t.boolean    "auto_next"            , null: false , default: true
      # t.string     "label_format"         , null: false , default: ''
class ProgramSeries < ApplicationRecord
  self.table_name="program_series"

  belongs_to :channel
  has_many :reservations
  has_one :program_series_dayoff
  minimum :start_at, 0
  maximum :start_at, 24 * 3600 - 1
  minimum :weekdays, 0
  maximum :weekdays, 0b1111111
  minimum :next_episode_number, 1
  minimum :last_episode_number, 1

  before_create -> {
    if not self.expire_date
      self.expire_date = self.begin_on
      self.expire_date_enable = false
    end
  }

  def next_datetime(include_today=true)
    now = Time.now
    t = now.beginning_of_day + self.start_at
    if Weekdays.include?(now.wday, self.weekdays) and (now < t)
      t
    else
      Weekdays.nearest_date(t,self.weekdays).to_time.beginning_of_day + self.start_at
    end
  end

  def label
    self.label_format || self.name
  end

  def self.find_by(channel_id, st, duration_sec,  wday_mask, name="", desc="")
    itime = st.get_itime
    self.where(channel_id: channel_id)
      .where("start_at <= ? and ? <= start_at + duration", itime, itime)
      .where("weekdays & ? > 0", wday_mask)
      .where("begin_on <= ?", st) # dont forget !!
      .order(begin_on: :desc)
      .first
  end

  def self.find_or_create_by(channel_id, start_time, duration_sec,  wday_mask, name="", desc="")
    itime = start_time.get_itime
    self.find_by(channel_id, start_time, duration_sec,  wday_mask, name, desc) || self.create(channel_id: channel_id, start_at: itime, weekdays: wday_mask, begin_on: start_time, duration: duration_sec, name: name || "", desc: desc || "")
  end

  def self.find_or_create_by_epg_program(ep)
    self.find_or_create_by(ep.channel_id, ep.start_time, ep.duration_sec, Weekdays.to_mask(ep.start_time.wday), ep.title, ep.desc)
  end

  def new_next_reservation
    st = self.next_datetime true
    return nil if not st
    epg = EpgProgram.where(channel_id: self.channel_id, start_time: st).first
    ed = st + self.duration
    title = epg ? epg.title : self.label
    desc  = epg ? epg.desc  : self.desc
    event_id = epg ? epg.event_id : -1

    Reservation.new(start_time: st,
                    stop_time: ed,
                    channel_id: self.channel_id,
                    program_series_id: self.id,
                    title: title,
                    desc: desc,
                    event_id: event_id,
                    counter: self.next_episode_number)
  end
end
