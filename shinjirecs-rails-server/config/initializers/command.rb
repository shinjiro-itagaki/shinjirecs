require 'pathname'
module Command
  module GetCommandPathResult
    GetSuccess    = Struct.new(:path)
    NotFound      = Struct.new(:path)
    NotExecutable = Struct.new(:path,:stat)
  end

  # : String
  def self.path
    Rails.root.join "config/commands"
  end

  # String -> GetCommandPathResult
  def self.get_command_path(relfilepath)
    cmdfile = self.path.to_s + "/" + relfilepath
    if File.exists?(cmdfile) then
      stat = File.stat(cmdfile)
      if stat.executable? then
        GetCommandPathResult::GetSuccess.new(cmdfile)
      else
        GetCommandPathResult::NotExecutable.new(cmdfile,stat)
      end
    else
      GetCommandPathResult::NotFound.new(cmdfile)
    end
  end

  # GetCommandPathResult
  def self.scan_channel_cmd
    self.get_command_path "scan_channel.sh"
  end

  # GetCommandPathResult
  def self.epgdump_cmd
    get_command_path "epgdump.sh"
  end
end