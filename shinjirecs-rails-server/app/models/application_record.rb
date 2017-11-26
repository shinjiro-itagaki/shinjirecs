class ApplicationRecord < ActiveRecord::Base
  self.abstract_class = true

  @@tasks = {}

  def self.register_task(*args,&block)
    t = Thread.new do
      begin
        block.call args
      rescue
      ensure
        @@tasks[self.id]=nil
      end
    end
    @@tasks[t.id]=nil
  end

  after_initialize :set_default, if: :new_record?

  @@maximums = {}
  @@minimums = {}

  def self.maximum(column,value)
    column = column.to_sym
    @@maximums[column] = value.to_i
    validates column, length: { maximum: value }, numericality: { only_integer: true }
  end

  def self.maximum_of(column)
    column = column.to_sym
    @@maximums[column]
  end

  def self.minimum(column,value)
    column = column.to_sym
    @@minimums[column] = value.to_i
    validates column, length: { minimum: value }, numericality: { only_integer: true }
  end

  def self.minimum_of(column)
    column = column.to_sym
    @@minimums[column]
  end

  # nil -> permitted all default params
  # []  -> not permitted all default params
  # [a,...] -> permitted only these params
  def self.permitted_params_only
    nil
  end

  def self.default_unpermitted_params
    [self.primary_key.to_s, "updated_at","created_at"]
  end

  def self.unpermitted_params
    (self.default_unpermitted_params || [])
  end

  def self.permitted_params
    (self.permitted_params_only || self.columns_hash.keys).map(&:to_s) - self.unpermitted_params
  end

  def self.params_info
    info = {}
    cols = self.columns_hash

    self.permitted_params.each do |k|
      k = k.to_s
      if c = cols[k] then
        info[k] = {"type" => c.type, "nullable" => c.null, "default" => c.default, "limit" => c.limit, "precision" => c.precision, "scale" => c.scale }
        if max = (self.maximum_of k) then
          info[k]["maximum"] = max
        end
        if min = (self.minimum_of k) then
          info[k]["minimum"] = min
        end
        if enums = self.defined_enums[k]
          info[k]["list"] = enums.values
        end

        if recs = self.list_records_of(k.to_sym)
          info[k]["list"] = recs.map(&:as_json)
        end
      else
        info[k] = {}
      end
    end
    info
  end

  def self.default_all_proxy
    self.all
  end

  alias_method :orig_as_json, :as_json

  def as_json(options = nil)
    res = super(options)
    res.keys.each do |k|
      v = res[k]
      res[k] = v.to_f if v.kind_of? Time
    end
    self.class.reflections.keys.each do |key|
      v = self.send key
      if v.kind_of? ActiveRecord::Relation then
        v = v.to_a.map{|x| x.orig_as_json options }
      else
        v = v.orig_as_json(options)
      end
      res[key] = v
    end
    res
  end

  def self.list_records_of(sym)
    nil
  end

  def display
    self.id.to_s
  end

  protected
  def set_default
  end

end
