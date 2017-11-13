class EpgdumpSchedule < ApplicationRecord
  belongs_to :system
  validates :weekdays, length: { minimum: 0, maximum: 0b1111111 }, numericality: { only_integer: true }
end
