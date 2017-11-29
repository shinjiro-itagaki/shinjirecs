class Time
  def get_itime
    hh = self.hour
    mm = self.min
    ss = self.sec
    hh * 3600 + mm * 60 + ss
  end
end
