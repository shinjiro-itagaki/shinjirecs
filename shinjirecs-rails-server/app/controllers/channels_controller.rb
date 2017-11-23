require 'timeout'

class ChannelsController < ApplicationController
  set_model Channel
  def self.permitted_params
    [:number,:area_id,:ctype,:display_name,:order]
  end

  def self.scan_timeout
    8
  end

  def scan
    timeout_sec = params[:timeout].to_s.to_i
    timeout_sec = self.class.scan_timeout if timeout_sec < 1

    gr = params[:gr] || []
    bs = params[:bs] || []

    area_id = System.instance.area_id
    cmdfile = Rails.root.to_s + "/config/commands/scan_channel.sh"
    if !File.exists?(cmdfile)
      puts cmdfile + " is not found."
      render_data nil, system: System.instance, setup: false
      return
    end

    {"gr" => gr,"bs" => bs}.each do |type,charr|
      charr = charr.map {|ch|
        c = Channel.find_or_initialize_by(number: ch, area_id: area_id)
        if c.new_record?
          c.scaned = false
          c.save
        end
        c
      }

      charr.each do |ch|
        cmd = "#{cmdfile} #{ch.number}"
        puts cmd
        res = false
        begin
          Timeout.timeout(timeout_sec) do
            res = system cmd
          end
        rescue
        end

        if res then
          puts "command success"
          puts $?
          ch.enable = true
        else
          ch.enable = false
          puts "command failed"
        end
        ch.scaned = true
        ch.save
      end
    end
  end

  protected
  def system_setup_check
    if not System.instance
      render_data nil, system: System.instance, setup: false
    end
  end
end
