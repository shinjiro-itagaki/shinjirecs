class EpgProgramsController < ApplicationController
  set_model EpgProgram
  def self.permitted_params
    [:start_time, :stop_time, :channel_id, :title, :desc, :epg_program_category_id]
  end
end
