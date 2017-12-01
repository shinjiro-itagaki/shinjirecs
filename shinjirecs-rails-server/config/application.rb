require_relative 'boot'

require "rails"
# Pick the frameworks you want:
require "active_model/railtie"
require "active_job/railtie"
require "active_record/railtie"
require "action_controller/railtie"
require "action_mailer/railtie"
require "action_view/railtie"
require "action_cable/engine"
# require "sprockets/railtie"
require "rails/test_unit/railtie"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module ShinjirecsRailsServer
  class Application < Rails::Application
    # Initialize configuration defaults for originally generated Rails version.
    config.load_defaults 5.1

    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Only loads a smaller set of middleware suitable for API only apps.
    # Middleware like session, flash, cookies can be added back manually.
    # Skip views, helpers and assets when generating a new resource.
    config.api_only = true
    config.video_path = config.paths.path + "public/videos"
    config.video_access_path = config.video_path
    config.time_zone = 'Tokyo'

    def wakeup_or_start_observer_thread
      if @observer_thread then
        if @observer_thread.alive? and @observer_thread.status != "aborting" then
          @observer_thread.wakeup
        else
          @observer_thread = nil
        end
      end

      @observer_thread ||= Thread.start(Thread.current) do |pth|
        while true
          if ActiveRecord::Base.connection.pool.connected? then
            ActiveRecord::Base.connection_pool.with_connection do
              if not pth.status or pth.status == "aborting" then
                break
              end
              self.class.once_observe_reservations(pth)
            end
          else
            sleep 1
          end
        end
      end
    end

    def self.once_observe_reservations(pth)
      begin
        sleepsec = nil
        if ActiveRecord::Base.connection.pool.connected? then
          if (next_check_time = Reservation.check_staging).kind_of? Time and (sec = next_check_time - Time.now) > 0 then
            sleepsec = sec
          end
        else
          sleepsec = 3
        end

        if sleepsec then
          sleep sleepsec
        else
          sleep
        end
      rescue => e
        puts e
      end

      puts "on staging reservations"
      puts Reservation.stagings.map{|r| ["id=#{r.id}", "stop_time=" + r.stop_time.to_s].join ",  "}
    end
  end
end
