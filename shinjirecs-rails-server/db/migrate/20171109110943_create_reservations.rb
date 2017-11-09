class CreateReservations < ActiveRecord::Migration[5.1]
  def change
    create_table :reservations, unsigned: true do |t|
      t.timestamp  "start_time"        , null: false
      t.integer    "duration"          , null: false
      t.references :channels           , null: false , foreign_key: true
      t.string     "title"             , null: false
      t.text       "desc"              , null: false
      t.integer    "event_id"          , null: false , default: 0
      t.references :program_categories , null: false , foreign_key: true
      t.timestamps                       null: false
    end
  end
end
