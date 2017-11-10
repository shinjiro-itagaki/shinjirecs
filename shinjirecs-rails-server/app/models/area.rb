class Area < ApplicationRecord
  has_many :channels
  # t.index ["label"], unique: true
end
