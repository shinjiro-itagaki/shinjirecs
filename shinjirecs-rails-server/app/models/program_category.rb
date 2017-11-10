class ProgramCategory < ApplicationRecord
  has_many :programs
  has_many :program_titles
  # t.index "label", unique: true
end
