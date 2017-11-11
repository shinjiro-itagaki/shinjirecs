class SystemsController < ApplicationController
  set_model System
  def root
    render_data system: System.get_instance
  end
end
