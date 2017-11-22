class ProgramTitleDayoffsController < ApplicationController
  set_model ProgramTitleDayoff
  set_parent_model ProgramTitle, :program_title_id
  def self.permitted_params
    [:on]
  end
end
