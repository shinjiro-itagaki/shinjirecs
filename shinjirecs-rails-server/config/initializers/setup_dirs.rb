path = Rails.application.config.video_path
testfile = path + "./__write_test__"
if not File.exists?(path) then
  Dir.mkdir path
end

if not File.directory?(path) then
  throw "#{path} is not directory"
end

begin
  File.unlink testfile if File.exists?(testfile)
  File.write(testfile,"")
rescue
  throw "#{path} is not writable."
end
