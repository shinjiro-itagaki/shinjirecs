class ProgramSeriesTerm < ApplicationRecord
  belongs_to :program_series
  validate do |rec|
    rec.errors[:finish_on] << "inconsistent finish date" if not rec.begin_on <= rec.finish_on
  end
end
