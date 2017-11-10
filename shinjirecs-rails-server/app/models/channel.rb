class Channel < ApplicationRecord
  # execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_ctype  CHECK( ctype IN ('gr','bs'));"
  # execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_number CHECK( number > 0 );"
  enum ctype: [:gr,:bs]
  belongs_to :area
  has_many :reservations
  has_many :program_titles
  has_many :programs
end
