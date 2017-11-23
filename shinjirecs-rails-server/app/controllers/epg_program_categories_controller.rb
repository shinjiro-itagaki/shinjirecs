class EpgProgramCategoriesController < ApplicationController
  set_model EpgProgramCategory
  def self.permitted_params
    [:label_ja,:label_en]
  end
end
