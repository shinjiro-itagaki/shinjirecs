require 'timeout'
require 'open3'
require 'tempfile'

class ChannelsController < ApplicationController
  set_model Channel
  def self.permitted_params
    [:number,:area_id,:ctype,:display_name,:display_name_locked,:order]
  end

  def self.scan_timeout
    20
  end

  def scan
    timeout_sec = params[:timeout].to_s.to_i
    timeout_sec = self.class.scan_timeout if timeout_sec < 1

    gr = params[:gr] || Channel.default_all_gr_numbers
    bs = params[:bs] || Channel.default_all_bs_numbers
    area_id = System.instance.area_id
    cmdfile = Command.scan_channel_cmd
    cmdfilepath = cmdfile.path

    case cmdfile
    when Command::GetCommandPathResult::GetSuccess
    when Command::GetCommandPathResult::NotFound
      puts cmdfilepath + " is not found."
      render_data nil
      return
    when Command::GetCommandPathResult::NotExecutable
      puts cmdfilepath + " is not executable."
      render_data nil
      return
    end

    {"gr" => gr,"bs" => bs}.each do |type,charr|
      charr = charr.map {|ch|
        c = Channel.find_or_initialize_by(number: ch, area_id: area_id)
        if c.new_record?
          c.scaned = false
          c.save!
        end
        c
      }

      charr.each do |ch|
        tempfile = Tempfile.new('for-scan')
        tempfilepath = tempfile.path
        cmd = "#{cmdfilepath} #{ch.number} #{timeout_sec} #{tempfilepath}"
        tempfile.unlink

        puts cmd
        res = false
        scaned = true
        # io = IO.popen(cmd, "r")
        pid = spawn(cmd, pgroup: Process.pid)  # io.pid
        puts "command pid=#{pid}"
        watch_thread = Process.detach(pid)
        watch_thread.join

        if File.exists?(tempfilepath) then
          puts "command success  ......... channel '#{ch.number}' was DETECTED!! "
          ch.exist = true
        else
          puts "command failed. Channel '#{ch.number}' was not found."
          ch.exist = false
        end
        ch.scaned = scaned
        puts "before save=="
        ch.save!
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
