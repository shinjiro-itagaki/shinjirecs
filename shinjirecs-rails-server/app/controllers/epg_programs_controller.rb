require 'json'
class EpgProgramsController < ApplicationController
  set_model EpgProgram
  before_action :set_record, only: [:new_reservation]

  class EpgProgramDompThread < Thread
    attr_accessor :stop
  end

  # params: {channels: []}
  # app.post "/epg_programs/epgdump", params: {in: ["~/test.ts"]}
  def epgdump
    namespace = "epg_programs_controller#epgdump"
    res = nil
    if params["stop"] then
      res = self.class.stop_thread namespace
    else
      res = self.class.run_thread(namespace, params["in"], params["sec"]) do |inn, sec|
        EpgProgram.epgdump(inn,sec)
      end
    end
    render_data result: res
  end

  def new_reservation
    @record = @record.new_reservation
    if @record.save
      render_data @record, status: :created, location: @record
    else
      render_data @record.errors, status: :unprocessable_entity
    end
  end

  def self.permitted_params
    [:start_time, :stop_time, :channel_id, :title, :desc, :epg_program_category_id]
  end
end
