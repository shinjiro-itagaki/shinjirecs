class ProgramTitlesController < ApplicationController
  set_model ProgramTitle
  def self.permitted_params
    [:start_at,:duration,:channel_id,:title,:desc,:next_counter,:weekdays,:auto_next,:label_format]
  end
end
