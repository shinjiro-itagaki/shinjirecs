class EpgdumpSchedulesController < ApplicationController
  set_model EpgdumpSchedule
  def self.permitted_params
    [:system_id,:time,:weekdays]
  end
end
