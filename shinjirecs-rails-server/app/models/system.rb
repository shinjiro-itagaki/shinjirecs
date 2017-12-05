class System < ApplicationRecord
  belongs_to :area
  has_many :epgdump_schedules
  scope :where_instance, ->(){ where(active: true ) }
  scope :where_dummy   , ->(){ where(active: false) }

  @@instance = nil

  after_commit :modify_db_connection_pools_count_if_need
  after_commit :refresh_instance

  min = 0
  minimum :gr_tuner_count, min
  minimum :bs_tuner_count, min
  minimum :busy_gr_tuner_count, min
  minimum :busy_bs_tuner_count, min

  validate do |rec|
    rec.errors[:busy_gr_tuner_count] << "inconsistent busy_gr_tuner_count" if rec.gr_tuner_count < rec.busy_gr_tuner_count
    rec.errors[:busy_bs_tuner_count] << "inconsistent busy_bs_tuner_count" if rec.bs_tuner_count < rec.busy_bs_tuner_count
    rec.errors[:gr_tuner_count]      << "it cannot GR tuner count" if not Reservation.tuner_count_changeable?(:gr, rec.gr_tuner_count)
    rec.errors[:bs_tuner_count]      << "it cannot BS tuner count" if not Reservation.tuner_count_changeable?(:bs, rec.bs_tuner_count)
  end

  def self.reload_instance() @@instance = self.where_instance.first  end

  def self.instance()
    res = @@instance || self.reload_instance
    res.modify_db_connection_pools_count_if_need
    res
  end
  def self.ins() self.instance end

  def self.setup_finished?
    ins = self.instance
    ins && ins.setup?
  end

  def tuner_count(ctype)
    case ctype
    when Channel.ctypes[:gr]
      return self.gr_tuner_count
    when Channel.ctypes[:bs]
      return self.bs_tuner_count
    end
    return 0
  end

  def channels
    self.area.channels
  end

  def new_channel
    return if not self.area_id
    Channel.new(area_id: self.area_id)
  end

  def find_or_initialize_channel_by_sid(service_id)
    Channel.find_or_initialize_by(service_id: service_id, area_id: self.area_id)
  end

  def find_or_initialize_channel_by_number(chnum)
    Channel.find_or_initialize_by(number: chnum, area_id: self.area_id)
  end

  def self.list_records_of(sym)
    case sym
    when :area_id then
      Area.all.to_a
    else
      nil
    end
  end

  def rest_tuner_count(ctype)
    case ctype
    when Channel.ctypes[:gr]
      return self.rest_gr_tuner_count
    when Channel.ctypes[:bs]
      return self.rest_bs_tuner_count
    end
  end

  def rest_bs_tuner_count
    self.bs_tuner_count - self.busy_bs_tuner_count
  end

  def rest_gr_tuner_count
    self.gr_tuner_count - self.busy_gr_tuner_count
  end

  @@orig_conf = ActiveRecord::Base.configurations[Rails.env]
  @@mutex = Mutex.new

  def modify_db_connection_pools_count_if_need
    all_tuners_count = Channel.ctypes.values.map{|type|
      self.tuner_count(type)
    }.sum
    conf = @@orig_conf.dup
    newsize = (conf["pool"] += all_tuners_count)
    if ActiveRecord::Base.connection.pool.size != newsize then
      ActiveRecord::Base.establish_connection(conf)
    end
  end

  def self.use_tuner(ctype,&block)
    rest = 0
    col = nil
    count = nil
    ins = System.instance

    begin
      @@mutex.lock
      case ctype
      when Channel.ctypes[:bs] then
        rest = rest_bs_tuner_count
        col = "busy_bs_tuner_count"
      when Channel.ctypes[:gr] then
        rest = rest_gr_tuner_count
        col = "busy_gr_tuner_count"
      end
      if rest > 0 then
        ins.update(col => ins.attributes[col] + 1)
      end
    ensure
      @@mutex.unlock
    end

    if rest > 0 then
      ins.update(col => ins.attributes[col] + 1)
      begin
        block.call
        ins.reload
      rescue => e
        throw e
      ensure
        ins.update(col => ins.attributes[col] - 1)
      end
    else
      false
    end
  end

  def find_or_create_epgdump_schedule(params)
    self.epgdump_schedules.find_or_create_by(params)
  end

  def del_epgdump_schedules(ids)
    recs = self.epgdump_schedules.where(id: ids).to_a
    recs.each(&:destroy)
    recs.select(&:destroyed?).map(&:id)
  end

  private
  def refresh_instance
    if @@instance and !(@@instance.object_id == self.object_id)
      @@instance.reload
    end
  end
end
