# Load the Rails application.
require_relative 'application'

# Initialize the Rails application.
Rails.application.initialize!

OBSERVER_THREAD = Thread.new do
  while true
    begin
      if ActiveRecord::Base.connection.pool.connected? then
        Reservation.check_staging
        sleep 1
      else
        sleep 3
      end
    rescue => e
      puts e.message
    end
  end
end
