# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20171109170533) do

  create_table "areas", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "label", null: false
    t.boolean "channels_checked", default: false, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["label"], name: "index_areas_on_label", unique: true
  end

  create_table "attachinfos", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "desc", null: false
  end

  create_table "audio_types", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "typ", null: false
    t.string "langcode", null: false
    t.string "extdesc", default: "", null: false
  end

  create_table "channels", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "number", null: false
    t.integer "service_id", null: false
    t.integer "area_id", default: 0, null: false
    t.string "ctype", default: "gr", null: false
    t.string "display_name", null: false
    t.boolean "display_name_locked", default: false, null: false
    t.integer "order", default: 0, null: false
    t.boolean "enable", default: false, null: false
    t.boolean "exist", default: false, null: false
    t.boolean "scaned", default: false, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["area_id", "service_id"], name: "index_channels_on_area_id_and_service_id", unique: true
  end

  create_table "epg_program_attachinfo_maps", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_id", null: false
    t.integer "attachinfo_id", null: false
    t.index ["program_id", "attachinfo_id"], name: "unique_epg_program_attachinfo_maps", unique: true
  end

  create_table "epg_program_audio_type_maps", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_id", null: false
    t.integer "audio_type_id", null: false
    t.index ["program_id", "audio_type_id"], name: "unique_epg_program_audio_type_maps", unique: true
  end

  create_table "epg_program_categories", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "label_ja", null: false
    t.string "label_en", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["label_ja", "label_en"], name: "index_epg_program_categories_on_label_ja_and_label_en", unique: true
  end

  create_table "epg_program_category_maps", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_id", null: false
    t.integer "category_id", null: false
    t.index ["program_id", "category_id"], name: "unique_epg_program_category_maps", unique: true
  end

  create_table "epg_program_medium_categories", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "label_ja", null: false
    t.string "label_en", null: false
    t.integer "parent_id", default: 0, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["parent_id", "label_ja", "label_en"], name: "unique_epg_program_medium_categories", unique: true
  end

  create_table "epg_program_medium_category_maps", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_id", null: false
    t.integer "category_id", null: false
    t.index ["program_id", "category_id"], name: "unique_epg_program_medium_category_maps", unique: true
  end

  create_table "epg_program_video_type_maps", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_id", null: false
    t.integer "video_type_id", null: false
    t.index ["program_id", "video_type_id"], name: "unique_epg_program_video_type_maps", unique: true
  end

  create_table "epg_programs", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.datetime "start_time", null: false
    t.datetime "stop_time", null: false
    t.integer "channel_id", null: false
    t.string "title", null: false
    t.text "desc", null: false
    t.integer "event_id", default: 0, null: false
    t.boolean "freeCA", default: false, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["start_time", "channel_id"], name: "index_epg_programs_on_start_time_and_channel_id", unique: true
  end

  create_table "epgdump_schedules", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "system_id", null: false
    t.time "time", default: "2000-01-01 00:00:00", null: false
    t.integer "weekdays", limit: 1, default: 127, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "program_title_dayoffs", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_title_id", null: false
    t.date "on", null: false
    t.index ["program_title_id", "on"], name: "index_program_title_dayoffs_on_program_title_id_and_on", unique: true
  end

  create_table "program_title_terms", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "program_title_id", null: false
    t.date "begin_on", null: false
    t.date "finish_on", null: false
    t.index ["program_title_id"], name: "index_program_title_terms_on_program_title_id", unique: true
  end

  create_table "program_titles", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.time "start_at", null: false
    t.integer "duration", null: false
    t.integer "channel_id", null: false
    t.string "title", null: false
    t.text "desc", null: false
    t.integer "next_counter", default: 1, null: false
    t.integer "weekdays", limit: 1, default: 0, null: false
    t.boolean "auto_next", default: true, null: false
    t.string "label_format", default: "", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "reservations", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.datetime "start_time", null: false
    t.integer "duration", null: false
    t.integer "channel_id", default: 0, null: false
    t.integer "program_title_id", default: 0, null: false
    t.string "title", null: false
    t.text "desc", null: false
    t.integer "event_id", default: 0, null: false
    t.integer "counter", default: 0, null: false
    t.integer "state", limit: 1, default: 0, null: false
    t.text "command_str", null: false
    t.integer "command_pid", null: false
    t.text "log", null: false
    t.text "errror_log", null: false
    t.string "filename", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "systems", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "area_id", default: 0, null: false
    t.boolean "active", default: true, null: false
    t.boolean "setup", default: false, null: false
    t.integer "gr_tuner_count", limit: 1, default: 1, null: false
    t.integer "bs_tuner_count", limit: 1, default: 0, null: false
    t.integer "rest_gr_tuner_count", limit: 1, default: 1, null: false
    t.integer "rest_bs_tuner_count", limit: 1, default: 0, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["active"], name: "index_systems_on_active", unique: true
  end

  create_table "video_types", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "resolution", null: false
    t.string "aspect", null: false
  end

end
