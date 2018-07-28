# Load the Rails application.
require_relative 'application'

# Initialize the Rails application.
Rails.application.initialize!
if ActiveRecord::Base.connection.pool.connected? then
  puts "connection size is #{ActiveRecord::Base.connection.pool.size}"
  puts "new connection size is #{ActiveRecord::Base.connection.pool.size}"  
  if System.table_exists? then
    System.instance # load system and set new connection pools
    Rails.application.wakeup_or_start_observer_thread
    Rails.application.wakeup_or_start_epgdump_thread
  end
else
  puts "no connection now"
end
