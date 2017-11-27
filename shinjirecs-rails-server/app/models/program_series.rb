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
  minimum :start_at, 0
  maximum :start_at, 24 * 3600 - 1
  minimum :weekdays, 0
  maximum :weekdays, 0b1111111
  minimum :next_episode_number, 1
  minimum :last_episode_number, 1


  def next_datetime(include_today=true)
    now = Time.now
    t = now.beginning_of_day + self.start_at
    if Weekdays.include?(now.wday, self.weekdays) and (now < t)
      t
    else
      Weekdays.nearest_date(t,self.weekdays)
    end
  end

  def new_reservation
    
  end
end
