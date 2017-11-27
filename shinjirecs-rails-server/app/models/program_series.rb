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
      Weekdays.nearest_date(t,self.weekdays)
    end
  end

  def label
    self.label_format || self.name
  end

  def self.find_or_create_by_epg_program(ep)
    chid = ep.channel_id
    st = ep.start_time
    hh = st.hour
    mm = st.min
    ss = st.sec
    wday = st.wday
    wday_mask = Weekdays.to_mask(wday)
    itime = hh * 3600 + mm * 60 + ss
    ins = self.where(channel_id: chid, start_at: itime)
      .where(["weekdays & ? > 0", wday_mask])
      .where(["begin_on <= ?", st]) # dont forget !!
      .order(begin_on: :desc)
      .first
    ins || self.create(channel_id: chid, start_at: itime, weekdays: wday_mask, begin_on: st, duration: ep.duration_sec, name: ep.title, desc: "")
  end

  def new_next_reservation(epg=nil)
    st = self.next_datetime true
    return nil if not st
    epg ||= EpgProgram.where(channel_id: self.channel_id, start_time: st).first

    title = epg ? epg.title : self.label
    desc  = epg ? epg.desc  : self.desc
    event_id = epg ? epg.event_id : -1

    Reservation.new(start_time: st,
                    stop_time: st + self.duration,
                    channel_id: self.channel_id,
                    program_series_id: self.id,
                    title: title,
                    desc: desc,
                    event_id: event_id,
                    counter: self.next_episode_number)
  end
end
