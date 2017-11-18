class SystemsController < ApplicationController
  set_model System
  def root
    render_data system: System.get_instance
  end

  def index
    render_data @model.instance
  end

  private
  def system_setup_check
  end
end
