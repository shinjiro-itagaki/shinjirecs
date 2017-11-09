class CreateInitTables < ActiveRecord::Migration[5.1]
  def change
    create_table :channels, unsigned: true do |t|
      t.integer "number"       , null: false
      t.string  "ctype"        , null: false, default: "gr"
      t.string  "display_name" , null: false
      t.integer "order"        , null: false, default: 0
      t.timestamps               null: false
    end
    execute "ALTER TABLE channels     ADD CONSTRAINT chk_channel_ctype     CHECK( ctype IN ('gr','bs'));"

    create_table :program_categories, unsigned: true do |t|
      t.string     "label" , null: false
      t.timestamps           null: false
    end

    create_table :programs, unsigned: true do |t|
      t.timestamp  "start_time"           , null: false
      t.timestamp  "stop_time"            , null: false
      t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :cascade}
      t.string     "title"                , null: false
      t.text       "desc"                 , null: false
      t.integer    "event_id"             , null: false , default: 0
      t.integer    "program_category_id"  , null: false , foreign_key: true
      t.timestamps                          null: false
    end

    create_table :reservations, unsigned: true do |t|
      t.timestamp  "start_time"          , null: false
      t.integer    "duration"            , null: false
      t.integer    "channel"             , null: false , foreign_key: true
      t.string     "title"               , null: false
      t.text       "desc"                , null: false
      t.integer    "event_id"            , null: false , default: 0
      t.integer    "program_category_id" , null: false , foreign_key: true

      # -2: canceled
      # -1: failed
      #  0: waiting
      #  1: recording
      #  2: success
      t.integer    "state"               , null: false , default: 0
      t.timestamps                         null: false
    end
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_state CHECK( state IN (-2,-1,0,1,2));"
  end
end
