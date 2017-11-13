class ProgramMediumCategory < ApplicationRecord
  belongs_to :program_category, foreign_key: :parent_id, class_name: "ProgramCategory"
  validates_uniqueness_of :label_ja
  validates_uniqueness_of :label_en
end
