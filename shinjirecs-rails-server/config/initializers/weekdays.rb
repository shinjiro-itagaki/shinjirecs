module Weekdays
  Sun = 0b0000001
  Mon = 0b0000010
  Tue = 0b0000100
  Wed = 0b0001000
  Thu = 0b0010000
  Fri = 0b0100000
  Sat = 0b1000000

  def self.all
    self.constants.map{|name| self.const_get(name) }
  end

  def self.none() 0 end

  def get_weekdays
  end
end
