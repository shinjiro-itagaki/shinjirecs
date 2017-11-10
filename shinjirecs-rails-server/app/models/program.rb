class Program < ApplicationRecord
  # execute "ALTER TABLE programs ADD CONSTRAINT chk_times CHECK( start_time < stop_time );"

  belongs_to :channel
  belongs_to :program_category

  def overlap?(start,endt)
    st = self.starttime
    et = self.stoptime
    !( ( start > et and endt > et ) or ( start < st and endt < st ) )
  end

  def include?(time)
    self.starttime <= time and time <= self.stoptime
  end

  def length
    (( self.stoptime - self.starttime ) / 60).to_i
  end

  def reservation
    r = Reservations.new(self.starttime,
                         self.stoptime,
                         self.channelinfo)

    @reservation ||= Reservations.find(r.id)[0]
  end
end
