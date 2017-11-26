require 'json'
class EpgProgramsController < ApplicationController
  set_model EpgProgram

  def self.epgdump(channel_numbers_or_filepaths=nil, sec=20)
    channel_numbers_or_filepaths ||= Channel.default_all_numbers

    cmdfile = Command.epgdump_cmd
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

    res = {}
    channel_numbers_or_filepaths.map do |chnum_or_fpath|
      #  in(ch) out sec
      chnumber = File.exists?(File.expand_path(chnum_or_fpath.to_s)) ? nil : chnum_or_fpath
      cmd = "#{cmdfilepath} #{chnum_or_fpath} - #{sec}"
      puts cmd
      begin
        json = JSON.parse(`#{cmd}`)
      rescue
        # json parse error
        puts e
      end
      puts json.length
      EpgProgram.import_epg(json,chnumber)
      res[chnum_or_fpath]=json
    end
    res
  end

  # params: {channels: []}
  # app.post "/epg_programs/epgdump", params: {in: ["~/test.ts"]}
  def epgdump
    render_data self.class.epgdump(params["in"], params["sec"])
  end

  def self.permitted_params
    [:start_time, :stop_time, :channel_id, :title, :desc, :epg_program_category_id]
  end
end
