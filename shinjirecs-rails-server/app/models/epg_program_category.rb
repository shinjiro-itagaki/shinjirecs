class EpgProgramCategory < ApplicationRecord
  has_many :epg_program_medium_category, foreign_key: 'parent_id'
  validates_uniqueness_of :label_ja, scope: :label_en

  def mediums_proxy
    self.epg_program_medium_category
  end
end
