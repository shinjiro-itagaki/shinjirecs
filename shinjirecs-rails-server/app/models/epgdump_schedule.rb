class EpgdumpSchedule < ApplicationRecord
  belongs_to :system
  maximum :weekdays, 0b1111111
  minimum :weekdays, 0
  minimum :time, 0
  maximum :time, 24 * 3600 - 1

  scope :today, ->(system_id) { self.where(system_id: system_id).where("weekdays & ? > 0", Time.now.wday) }

  class EpgdumpThread < Thread
    attr_accessor :task_count, :finished_count, :failed_count, :skipped_count, :start_time, :duration
    
    def progress_percent
      if @task_count.to_i == 0 then
        nil
      else
        (1000 * @finished_count.to_i / @task_count.to_i).to_f / 10.0
      end
    end

    def message
      "#{self.progress_percent}% ( #{self.finished_count} / #{self.task_count} )"
    end
  end
  
  def today
    Time.now.beginning_of_day + self.time
  end

  def self.today_times(system_id)
    self.today(system_id).to_a.map(&:time).sort
  end

  @@epgdump_thread = nil

  def self.epgdump_thread
    @@epgdump_thread
  end

  def self.start_epgdump_thread(duration=120)
    if @@epgdump_thread and @@epgdump_thread.status then
      return @@epgdump_thread.message
    end

    @@epgdump_thread = EpgdumpThread.new duration do |d|
      channels = Channel.exist.to_a
      Thread.current.task_count = channels.count
      Thread.current.finished_count = 0
      Thread.current.failed_count = 0
      Thread.current.start_time = Time.now
      Thread.current.duration = d
      channels.sort_by{|ch|
        e = ch.epg_programs.order(updated_at: :desc).first
        (e ? e.updated_at : nil).to_i
      }.each do |ch|
        st = Time.now
        ed = st + d
        if not Reservation.full?(st,ed,ch.ctype) then
          begin
            if EpgProgram.epgdump(ch.number, d - 3) then
              Thread.current.finished_count += 1
            else
              Thread.current.failed_count += 1
            end
          rescue => e
            puts e
            puts e.backtrace
          end
        else
          Thread.current.skipped_count += 1
        end
      end
      Thread.current.task_count = 0
      Thread.current.finished_count = 0
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
end
