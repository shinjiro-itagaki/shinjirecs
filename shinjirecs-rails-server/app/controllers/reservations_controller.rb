class ReservationsController < ApplicationController
  set_model Reservation
  def self.permitted_params
    [:start_time,:duration,:channel_id,:program_title_id,:title,:desc,:counter,:state,:filename]
  end
end
