class CreateInitTables < ActiveRecord::Migration[5.1]
  def change
    create_table :areas, unsigned: true do |t|
      t.string  "label"            , null: false
      t.boolean "channels_checked" , null: false, default: false
      t.timestamps                   null: false
      t.index ["label"], unique: true
    end

    create_table :systems, unsigned: true do |t|
      t.integer "area_id"      , null: false, default: 0, foreign_key: {on_delete: :restrict, on_update: :cascade}
      t.boolean "active"       , null: false, default: true
      t.boolean "initialized"  , null: false, default: false
      t.integer "gr_tuner_count" , null: false, default: 1
      t.integer "bs_tuner_count" , null: false, default: 0
      t.integer "rest_gr_tuner_count" , null: false, default: 1
      t.integer "rest_bs_tuner_count" , null: false, default: 0
      t.index ["active"], unique: true
      t.timestamps               null: false
    end
    execute "ALTER TABLE systems ADD CONSTRAINT system_active CHECK( active = 1 );"
    execute "ALTER TABLE systems ADD CONSTRAINT system_tuner_counts CHECK( gr_tuner_count > -1 and bs_tuner_count > -1 and rest_gr_tuner_count > -1 and rest_bs_tuner_count > -1 and gr_tuner_count >= rest_gr_tuner_count and bs_tuner_count >= rest_bs_tuner_count);"

    create_table :channels, unsigned: true do |t|
      t.integer "number"       , null: false
      t.integer "area_id"      , null: false, default: 0, foreign_key: {on_delete: :restrict, on_update: :cascade}
      t.string  "ctype"        , null: false, default: "gr"
      t.string  "display_name" , null: false
      t.integer "order"        , null: false, default: 0
      t.timestamps               null: false
      t.index ["area_id","number","ctype"], unique: true
      t.index ["area_id","display_name"], unique: true
    end
    execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_ctype  CHECK( ctype IN ('gr','bs'));"
    execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_number CHECK( number > 0 );"

    create_table :program_categories, unsigned: true do |t|
      t.string     "label" , null: false
      t.timestamps           null: false
      t.index "label", unique: true
    end

    create_table :programs, unsigned: true do |t|
      t.timestamp  "start_time"           , null: false
      t.timestamp  "stop_time"            , null: false
      t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade} # delete all programs if channel is deleted
      t.string     "title"                , null: false
      t.text       "desc"                 , null: false
      t.integer    "event_id"             , null: false , default: 0
      t.integer    "program_category_id"  , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
      t.timestamps                          null: false
      t.index ["start_time", "channel_id"], unique: true
    end
    execute "ALTER TABLE programs ADD CONSTRAINT chk_times CHECK( start_time < stop_time );"

    create_table :program_titles, unsigned: true do |t|
      t.timestamp  "start_time"           , null: false
      t.timestamp  "end_time"             , null: false
      t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :restrict, on_update: :cascade}
      t.string     "title"                , null: false
      t.text       "desc"                 , null: false
      t.integer    "program_category_id"  , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
      t.integer    "next_counter"         , null: false , default: 1
      t.integer    "weekdays"             , null: false , default: 0, limit: 1 # byte
      t.boolean    "auto_next"            , null: false , default: true
      t.string     "label_format"         , null: false , default: ''
      t.timestamps                          null: false
      t.index ["start_time", "channel_id"], unique: true
    end
    execute "ALTER TABLE program_titles ADD CONSTRAINT chk_between CHECK( start_time < end_time )"
    execute "ALTER TABLE program_titles ADD CONSTRAINT chk_weekdays CHECK( 0 <= weekdays and weekdays <= 127 )" # between ( 0b0000000 , 0b1111111 )
    execute "ALTER TABLE program_titles ADD CONSTRAINT chk_next_counter CHECK( next_counter > 0 )"

    create_table :reservations, unsigned: true do |t|
      t.timestamp  "start_time"          , null: false
      t.integer    "duration"            , null: false # seconds
      t.integer    "channel_id"          , null: false , default: 0, foreign_key: {on_delete: :restrict   , on_update: :cascade}
      t.integer    "program_title_id"    , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
      t.string     "title"               , null: false
      t.text       "desc"                , null: false
      t.integer    "event_id"            , null: false , default: 0
      t.integer    "counter"             , null: false , default: 0

      # -2: canceled
      # -1: failed
      #  0: waiting
      #  1: preparing
      #  2: recording
      #  3: success
      t.integer    "state"               , null: false , default: 0, limit: 1
      t.text       "command_str"         , null: false
      t.integer    "command_pid"         , null: false
      t.text       "log"                 , null: false
      t.text       "errror_log"          , null: false
      t.string     "filename"            , null: false
      t.timestamps                         null: false
    end
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_state CHECK( state IN (-2,-1,0,1,2,3));"
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_duration CHECK( duration > 0);"
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_counter CHECK( counter >= 0);"
  end
end
