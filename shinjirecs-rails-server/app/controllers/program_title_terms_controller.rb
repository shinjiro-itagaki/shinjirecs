class ProgramTitleTermsController < ApplicationController
  set_model ProgramTitleTerm
  set_parent_model ProgramTitle, :program_title_id
end
