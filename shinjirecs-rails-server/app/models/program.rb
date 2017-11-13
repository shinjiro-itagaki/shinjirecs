class Program < ApplicationRecord
  belongs_to :channel
  has_and_belongs_to_many :program_category,        association_foreign_key: "category_id", join_table: "program_category_maps"
  has_and_belongs_to_many :program_medium_category, association_foreign_key: "category_id", join_table: "program_medium_category_maps"

  validate do |rec|
    rec.errors[:stop_time] << "inconsistent stop_time" if not rec.start_time < rec.stop_time
  end

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
