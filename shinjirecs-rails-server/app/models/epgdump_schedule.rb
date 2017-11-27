class EpgdumpSchedule < ApplicationRecord
  belongs_to :system
  maximum :weekdays, 0b1111111
  minimum :weekdays, 0
  minimum :time, 0
  maximum :time, 24 * 3600 - 1
end
