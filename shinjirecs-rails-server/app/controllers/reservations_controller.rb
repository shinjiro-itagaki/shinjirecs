class ReservationsController < ApplicationController
  set_model Reservation
  def self.permitted_params
    [:start_time,:duration,:channel_id,:program_title_id,:title,:desc,:counter,:state,:filename]
  end

  protected
  
  def index_records_proxy(proxy)
    fl = filter
    if fl.include? "recently" then
      proxy = proxy.recently
    end
    if fl.include? "waiting" then
      proxy = proxy.will_record.order(start_time: :asc)
    end    
    proxy
  end

  def index_records(records)
    fl = filter
    if fl.include? "watchable" then
      records = records.select(&:watchable?)
    end
    records
  end

  def filter
    params["filter"].to_s.downcase
  end
end
