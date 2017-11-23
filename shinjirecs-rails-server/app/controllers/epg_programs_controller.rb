class EpgProgramsController < ApplicationController
  set_model EpgProgram

  def epgdump
    ch = params["ch"]
    register_task ch do |cint|
      stdout = `recpt1 --b25 --strip #{cint} 10 - | epgdump json - -`
      puts stdout
    end
  end

  def self.permitted_params
    [:start_time, :stop_time, :channel_id, :title, :desc, :epg_program_category_id]
  end
end
