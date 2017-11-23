class ProgramTitleTermsController < ApplicationController
  set_model ProgramTitleTerm
  set_parent_model ProgramTitle, :program_title_id
  def self.permitted_params
    [:begin_on,:finish_on]
  end
end
