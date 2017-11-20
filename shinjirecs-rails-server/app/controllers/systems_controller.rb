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

  private
  def system_setup_check
  end
end
