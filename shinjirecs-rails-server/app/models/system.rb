class System < ApplicationRecord
  belongs_to :area
  scope :where_instance, ->(){ where(active: true ) }
  scope :where_dummy   , ->(){ where(active: false) }

  @@instance = nil

  after_save :refresh_instance

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
  def self.instance() @@instance || self.reload_instance end
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

  private
  def refresh_instance
    if @@instance and !(@@instance.object_id == self.object_id)
      @@instance.reload
    end
  end
end
