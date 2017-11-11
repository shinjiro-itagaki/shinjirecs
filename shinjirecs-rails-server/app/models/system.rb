class System < ApplicationRecord
  belongs_to :area
  scope :instance, ->(){ where(active: true ) }
  scope :dummy   , ->(){ where(active: false) }

  @@instance = nil

  def self.reload_instance() @@instance  = self.instance.first  end
  def self.get_instance()    @@instance || self.reload_instance end

  # execute "ALTER TABLE system ADD CONSTRAINT system_active CHECK( active = true );"
  # execute "ALTER TABLE system ADD CONSTRAINT system_active CHECK( gr_tuner_count > -1 and bs_tuner_count > -1 and rest_gr_tuner_count > -1 and rest_bs_tuner_count > -1 and gr_tuner_count >= rest_gr_tuner_count and bs_tuner_count >= rest_bs_tuner_count);"
end
