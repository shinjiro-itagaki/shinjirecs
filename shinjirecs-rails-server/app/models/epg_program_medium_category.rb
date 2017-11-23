class EpgProgramMediumCategory < ApplicationRecord
  belongs_to :epg_program_category, foreign_key: :parent_id, class_name: "EpgProgramCategory"
  validates_uniqueness_of :label_ja
  validates_uniqueness_of :label_en
end
