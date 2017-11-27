# -- t.date       "begin_on"             , null: false
# -- t.date       "finish_on"            , null: false
# t.time       "start_at"             , null: false
# t.integer    "duration"             , null: false
# t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :restrict, on_update: :cascade}
# t.string     "title"    , null: false
# t.text "desc"     , null: false
# t.integer    "next_counter"   , null: false , default: 1
# t.integer    "weekdays" , null: false , default: 0, limit: 1 # byte
# t.boolean    "auto_next", null: false , default: true
# t.string     "label_format"   , null: false , default: ''
class ProgramSeries < ApplicationRecord
  self.table_name="program_series"
  
  belongs_to :channel
  belongs_to :program_category
  has_many :reservations
  # validates :weekdays, length: { minimum: 0, maximum: 0b1111111 }, numericality: { only_integer: true }
  minimum :weekdays, 0
  maximum :weekdays, 0b1111111
  # validates :next_counter, length: { minimum: 1 }, numericality: { only_integer: true }
  minimum :next_count, 1
#  validate do |rec|
#    rec.errors[:finish_on] << "inconsistent finish date" if not rec.begin_on < rec.finish_on
#  end

end
