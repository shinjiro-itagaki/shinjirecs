class Channel < ApplicationRecord
  # execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_ctype  CHECK( ctype IN ('gr','bs'));"
  # execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_number CHECK( number > 0 );"

  enum ctype: {gr: 'gr',bs: 'bs'}
  belongs_to :area
  has_many :reservations
  has_many :program_titles
  has_many :programs

  def self.minimum_number() 1 end

  validates :number, length: { minimum: self.minimum_number }, numericality: { only_integer: true }
  validates :display_name, presence: false

  def self.params_info
    s = super
    s["number"].merge "minimum" => self.minimum_number
    s
  end

  private
  def set_default
    self.ctype        ||= 'gr'
    self.display_name ||= "No Name " + Time.now.to_s(:number)
  end
end
