module Weekdays
  Sun = 0 # 0b0000001
  Mon = 1 # 0b0000010
  Tue = 2 # 0b0000100
  Wed = 3 # 0b0001000
  Thu = 4 # 0b0010000
  Fri = 5 # 0b0100000
  Sat = 6 # 0b1000000

  def self.all
    self.constants.map{|name| self.const_get(name) }.sort
  end

  def self.none() 0 end

  def self.include?(wday,weekdays_mask)
    mask = (1 << wday)
    (mask & weekdays_mask > 0)
  end

  def self.get_weekdays(i)
    return [] if not i.kind_of? Integer
    Weekdays.all.map {|wday|
      mask = 1 << wday
      (mask & i > 0) ? wday : nil
    }.compact
  end

  # does not include today, only after today
  def self.nearest_date(t,i)
    return nil if not t.kind_of? Time
    return nil if not i.kind_of? Integer
    return nil if i == 0
    wday = t.wday
    wdays = Weekdays.get_weekdays(i)
    wdays += wdays.map{|x|x+7}
    x = wdays.map{|x|x - wday}.select{|x| x > 0}.min
    (t + x.day).to_date
  end

  def self.nearest_from_now(i)
    self.nearest_date Time.now, i
  end
end
