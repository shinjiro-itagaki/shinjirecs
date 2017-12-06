class EpgdumpSchedule < ApplicationRecord
  belongs_to :system
  maximum :weekdays, 0b1111111
  minimum :weekdays, 0
  minimum :time, 0
  maximum :time, 24 * 3600 - 1

  scope :today, ->() { self.where("weekdays & ? > 0", Time.now.wday) }

  def today
    Time.now.beginning_of_day + self.time
  end

  @@epgdump_thread = nil

  def self.epgdump_thread
    @@epgdump_thread
  end

  def self.start_epgdump_thread(duration=60)
    if @@epgdump_thread and @@epgdump_thread.status then
      return nil
    end

    @@epgdump_thread = Thread.new duration do |d|
      Channel.exist.to_a.sort_by{|ch|
        e = ch.epg_programs.order(updated_at: :desc).first
        (e ? e.updated_at : nil).to_i
      }.each do |ch|
        st = Time.now
        ed = st + d
        if not Reservation.full?(st,ed,ch.ctype) then
          begin
            EpgProgram.epgdump(ch.number, d - 3)
          rescue => e
            puts e
            puts e.backtrace
          end
        end
      end
    end
  end

  def start
    return nil if not self.class.today.where(id: self.id, system_id: self.system_id).exists?
    today = self.today
    duration = today.sec
    st = today.beginning_of_minute()
    now = Time.now
    if st < now and now < today then
      self.class.start_epgdump_thread(duration)
    end
  end

  def self.last_updates
    res = {}
    Channel.exist.to_a.each do |ch|
      latest = ch.epg_programs.order(updated_at: :desc).first
      res[ch.id] = latest ? latest.updated_at : nil
    end
    res
  end
end
