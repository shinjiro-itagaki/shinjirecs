class ProgramTitle < ApplicationRecord
  belongs_to :channel
  belongs_to :program_category
  has_many :reservations
  # execute "ALTER TABLE programs ADD CONSTRAINT chk_between CHECK( start_time < end_time )"
  # execute "ALTER TABLE programs ADD CONSTRAINT chk_weekdays CHECK( 0 <= weekdays and weekdays <= 127 )" # between ( 0b0000000 , 0b1111111 )
  # execute "ALTER TABLE programs ADD CONSTRAINT chk_next_counter CHECK( next_counter > 0 )"
end
