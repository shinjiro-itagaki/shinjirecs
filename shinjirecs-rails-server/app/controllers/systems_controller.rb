class SystemsController < ApplicationController
  set_model System
  def all
    # render_data system: System.instance
    render_data ({
      :system                 => System.instance,
      :areas                  => Area.all,
      :channels               => Channel.all,
      :epg_programs           => EpgProgram.all,
      :epg_program_categories => EpgProgramCategory.all,
      :program_titles         => ProgramTitle.all,
      :reservations           => Reservation.all
    })
  end

  def index
    render_data @model.instance
  end

  def epgdump_schedules
    render_data @model.instance.epgdump_schedules
  end

  def add_epgdump_schedule
    render_data @model.instance.find_or_create_epgdump_schedule(params.permit(:time,:weekdays))
  end

  def del_epgdump_schedules
    render_data @model.instance.del_epgdump_schedules(params[:ids])
  end

  def self.permitted_params
    [:area_id,:setup,:gr_tuner_count,:bs_tuner_count]
  end

  private
  def system_setup_check
  end
end
