# Load the Rails application.
require_relative 'application'

# Initialize the Rails application.
Rails.application.initialize!

OBSERVER_THREAD = Thread.new do
  while true
    begin
      Reservation.check_staging
      sleep 1
    rescue => e
      puts e.message
    end
  end
end
