class ProgramCategory < ApplicationRecord
  has_many :program_medium_category, foreign_key: 'parent_id'
  validates_uniqueness_of :label_ja
  validates_uniqueness_of :label_en

  def mediums_proxy
    self.program_medium_category
  end
end
