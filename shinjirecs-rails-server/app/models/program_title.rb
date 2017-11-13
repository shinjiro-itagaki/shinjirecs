class ProgramTitle < ApplicationRecord
  belongs_to :channel
  belongs_to :program_category
  has_many :reservations
  validates :weekdays, length: { minimum: 0, maximum: 0b1111111 }, numericality: { only_integer: true }
  validates :next_counter, length: { minimum: 1 }, numericality: { only_integer: true }
  validate do |rec|
    rec.errors[:end_time] << "inconsistent end_time" if not rec.start_time < rec.end_time
  end
end
