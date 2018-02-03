# Load the Rails application.
require_relative 'application'

# Initialize the Rails application.
Rails.application.initialize!
if ActiveRecord::Base.connection.pool.connected? then
  puts "connection size is #{ActiveRecord::Base.connection.pool.size}"
  System.instance # load system and set new connection pools
  puts "new connection size is #{ActiveRecord::Base.connection.pool.size}"
else
  puts "no connection now"
end
Rails.application.wakeup_or_start_observer_thread
Rails.application.wakeup_or_start_epgdump_thread
