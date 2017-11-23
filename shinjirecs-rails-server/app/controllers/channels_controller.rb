class ChannelsController < ApplicationController
  set_model Channel
  def self.permitted_params
    [:number,:area_id,:ctype,:display_name,:order]
  end

  def scan
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
        c = ch.number
        cmd = "#{cmdfile} #{c}"
        puts cmd
        if system cmd then
          puts "command success"
          puts $?
          c.enable = true
        else
          c.enable = false
          puts "command failed"
        end
        c.scaned = true
        c.save
      end
    end
  end

  protected
  def system_setup_check
    if not System.instance
      render_data nil, system: ins, setup: false
    end
  end
end
