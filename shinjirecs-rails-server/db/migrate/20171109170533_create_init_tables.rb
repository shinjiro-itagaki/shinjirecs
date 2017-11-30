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
      t.boolean "setup"        , null: false, default: false
      t.integer "gr_tuner_count" , null: false, default: 1, limit: 1
      t.integer "bs_tuner_count" , null: false, default: 0, limit: 1
      t.integer "busy_gr_tuner_count" , null: false, default: 0
      t.integer "busy_bs_tuner_count" , null: false, default: 0
      t.index ["active"], unique: true
      t.timestamps               null: false
    end
    execute "ALTER TABLE systems ADD CONSTRAINT system_active CHECK( active = 1 );"
    execute "ALTER TABLE systems ADD CONSTRAINT system_tuner_counts CHECK( gr_tuner_count > -1 and bs_tuner_count > -1 and busy_gr_tuner_count > -1 and busy_bs_tuner_count > -1 and gr_tuner_count >= busy_gr_tuner_count and bs_tuner_count >= busy_bs_tuner_count);"

    create_table :epgdump_schedules, unsigned: true do |t|
      t.integer "system_id" , null: false, foreign_key: {on_delete: :cascade, on_update: :cascade}
      t.integer "time"      , null: false, default: 0
      t.integer "weekdays"  , null: false, default: 127, limit: 1 # byte, 0 means not active
      t.timestamps null: false
    end
    execute "ALTER TABLE epgdump_schedules ADD CONSTRAINT chk_schedule_time CHECK( time >= 0 and time < 86400  )"
    execute "ALTER TABLE epgdump_schedules ADD CONSTRAINT chk_schedule_weekdays CHECK( 0 <= weekdays and weekdays <= 127 )" # between ( 0b0000000 , 0b1111111 )

    create_table :channels, unsigned: true do |t|
      t.string  "number"       , null: false
      t.integer "service_id"   , null: false
      t.integer "area_id"      , null: false, default: 0, foreign_key: {on_delete: :restrict, on_update: :cascade}
      t.string  "ctype"        , null: false, default: "gr"
      t.string  "display_name" , null: false
      t.boolean "display_name_locked" , null: false, default: false
      t.integer "order"        , null: false, default: 0
      t.boolean "enable"       , null: false, default: false
      t.boolean "exist"        , null: false, default: false
      t.boolean "scaned"       , null: false, default: false
      t.timestamps               null: false
      t.index ["area_id","service_id"], unique: true
    end
    execute "ALTER TABLE channels ADD CONSTRAINT chk_channel_ctype  CHECK( ctype IN ('gr','bs'));"

    create_table :epg_program_categories, unsigned: true do |t|
      t.string "label_ja"   , null: false
      t.string "label_en"   , null: false
      t.timestamps            null: false
      t.index ["label_ja","label_en"], unique: true
    end

    create_table :epg_program_medium_categories, unsigned: true do |t|
      t.string "label_ja"   , null: false
      t.string "label_en"   , null: false
      t.integer "parent_id" , null: false , default: 0, foreign_key: {to_table: "epg_program_categories", on_delete: :set_default, on_update: :cascade}
      t.timestamps            null: false
      t.index ["parent_id","label_ja","label_en"], unique: true, name: "unique_epg_program_medium_categories"
    end

    create_table :video_types, unsigned: true do |t|
      t.string "resolution", null: false
      t.string "aspect",     null: false
    end

    create_table :audio_types, unsigned: true do |t|
      t.string "typ"      , null: false
      t.string "langcode" , null: false
      t.string "extdesc"  , null: false, default: ""
    end

    create_table :attachinfos, unsigned: true do |t|
      t.string "desc"     , null: false
    end

    create_table :epg_programs, unsigned: true do |t|
      t.datetime   "start_time"           , null: false
      t.datetime   "stop_time"            , null: false
      t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade} # delete all programs if channel is deleted
      t.string     "title"                , null: false
      t.text       "desc"                 , null: false
      t.integer    "event_id"             , null: false , default: 0
      t.boolean    "freeCA"               , null: false , default: false
      t.timestamps                          null: false
    end
    execute "ALTER TABLE epg_programs ADD CONSTRAINT chk_times CHECK( start_time < stop_time );"

    create_table :epg_program_category_maps, unsigned: true do |t|
      t.integer "program_id"  , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_programs"}
      t.integer "category_id" , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_program_categories"}
      t.index ["program_id","category_id"], unique: true, name: "unique_epg_program_category_maps"
    end

    create_table :epg_program_medium_category_maps, unsigned: true do |t|
      t.integer "program_id"  , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_programs"}
      t.integer "category_id" , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_program_medium_categories"}
      t.index ["program_id","category_id"], unique: true, name: "unique_epg_program_medium_category_maps"
    end

    create_table :epg_program_video_type_maps, unsigned: true do |t|
      t.integer "program_id"    , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_programs"}
      t.integer "video_type_id" , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "video_types"}
      t.index ["program_id","video_type_id"], unique: true, name: "unique_epg_program_video_type_maps"
    end

    create_table :epg_program_audio_type_maps, unsigned: true do |t|
      t.integer "program_id"    , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_programs"}
      t.integer "audio_type_id" , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "audio_types"}
      t.index ["program_id","audio_type_id"], unique: true, name: "unique_epg_program_audio_type_maps"
    end

    create_table :epg_program_attachinfo_maps, unsigned: true do |t|
      t.integer "program_id"    , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "epg_programs"}
      t.integer "attachinfo_id" , null: false , foreign_key: {on_delete: :cascade, on_update: :cascade, to_table: "attachinfos"}
      t.index ["program_id","attachinfo_id"], unique: true, name: "unique_epg_program_attachinfo_maps"
    end

    create_table :program_series, unsigned: true do |t|
      t.integer    "start_at"             , null: false
      t.integer    "duration"             , null: false
      t.integer    "channel_id"           , null: false , foreign_key: {on_delete: :restrict, on_update: :cascade}
      t.date       "begin_on"             , null: false
      t.date       "expire_date"          , null: false
      t.boolean    "expire_date_enable"   , null: false , default: false
      t.string     "name"                 , null: false
      t.boolean    "repeat"               , null: false , default: false
      t.text       "desc"                 , null: false
      t.integer    "next_episode_number"  , null: false , default: 1
      t.integer    "last_episode_number"  , null: false , default: 0
      t.integer    "weekdays"             , null: false , default: 0, limit: 1 # byte
      t.boolean    "auto_next"            , null: false , default: true
      t.string     "label_format"         , null: false , default: ''
      t.timestamps                          null: false
      t.index ["start_at","channel_id","begin_on"], unique: true, name: "idx_unique_program_series"
    end
    execute "ALTER TABLE program_series ADD CONSTRAINT chk_program_series_start_at CHECK( start_at >= 0 and start_at < 86400)"
    execute "ALTER TABLE program_series ADD CONSTRAINT chk_program_series_begin_on CHECK( begin_on <= expire_date )"
    execute "ALTER TABLE program_series ADD CONSTRAINT chk_weekdays CHECK( 0 <= weekdays and weekdays <= 127 )" # between ( 0b0000000 , 0b1111111 )
    execute "ALTER TABLE program_series ADD CONSTRAINT chk_next_episode_number CHECK( next_episode_number > 0 )"
    execute "ALTER TABLE program_series ADD CONSTRAINT chk_last_episode_number CHECK( last_episode_number >= 0 )"
    execute "ALTER TABLE program_series ADD CONSTRAINT chk_program_series_duration CHECK( duration > 0 )"

    create_table :program_series_dayoffs, unsigned: true do |t|
      t.integer "program_series_id" , null: false, foreign_key: {on_delete: :cascade, on_update: :cascade}
      t.date    "on"                , null: false
      t.index   ["program_series_id","on"], unique: true
    end

    create_table :reservations, unsigned: true do |t|
      t.datetime   "start_time"          , null: false
      t.datetime   "stop_time"           , null: false
      t.integer    "channel_id"          , null: false , default: 0, foreign_key: {on_delete: :restrict   , on_update: :cascade}
      t.integer    "program_series_id"   , null: false , default: 0, foreign_key: {on_delete: :set_default, on_update: :cascade}
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
      t.integer    "command_pid"         , null: false , default: 0
      t.text       "log"                 , null: false
      t.text       "error_log"           , null: false
      t.string     "filename"            , null: false , default: ""
      t.integer    "retried_count"       , null: false , default: 0
      t.timestamps                         null: false
    end
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_state CHECK( state IN (-2,-1,0,1,2,3));"
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_stop  CHECK( stop_time > start_time);"
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_counter CHECK( counter >= 0);"
    execute "ALTER TABLE reservations ADD CONSTRAINT chk_reservation_counter CHECK( retried_count >= 0);"
  end
end
