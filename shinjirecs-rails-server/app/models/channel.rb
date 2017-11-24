class Channel < ApplicationRecord
  enum ctype: {gr: 'gr',bs: 'bs'}
  belongs_to :area
  has_many :reservations
  has_many :program_titles
  has_many :programs

  before_save :auto_set_values

  scope :enables, ->(){ where(enable: true) }
  scope :exist,   ->(){ where(exist:  true) }
  scope :scaned,  ->(){ where(scaned: true) }

  def self.minimum_number() 1 end

  # validates :number, length: { minimum: self.minimum_number }, numericality: { only_integer: true }
  minimum :number, self.minimum_number
  validates :display_name, presence: false

  def self.params_info
    s = super
    s["number"].merge "minimum" => self.minimum_number
    s
  end

  private
  def set_default
    self.ctype        ||= 'gr'
    # self.display_name ||= "No Name " + Time.now.to_s(:number)
  end

  def auto_set_values
    if (self.display_name || "").empty?
      self.display_name = self.number
    end
  end

end
