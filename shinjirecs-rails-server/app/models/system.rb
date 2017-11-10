class System < ApplicationRecord
  belongs_to :area
  # execute "ALTER TABLE system ADD CONSTRAINT system_active CHECK( active = true );"
end
