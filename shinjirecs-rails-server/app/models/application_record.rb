class ApplicationRecord < ActiveRecord::Base
  self.abstract_class = true

  after_initialize :set_default, if: :new_record?

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
        info[k] = {"type" => c.type, "null" => c.null, "default" => c.default, "limit" => c.limit, "precision" => c.precision, "scale" => c.scale }
      else
        info[k] = {}
      end
    end
    info
  end

  def as_json(options = nil)
    res = super(options)
    res.keys.each do |k|
      v = res[k]
      res[k] = v.to_f if v.kind_of? Time
    end
    res
  end

  protected
  def set_default
  end

end
