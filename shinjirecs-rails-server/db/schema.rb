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

  create_table "channels", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.integer "number", null: false
    t.integer "area_id", default: 0, null: false
    t.string "ctype", default: "gr", null: false
    t.string "display_name", null: false
    t.integer "order", default: 0, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["area_id", "display_name"], name: "index_channels_on_area_id_and_display_name", unique: true
    t.index ["area_id", "number", "ctype"], name: "index_channels_on_area_id_and_number_and_ctype", unique: true
  end

  create_table "program_categories", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.string "label", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["label"], name: "index_program_categories_on_label", unique: true
  end

  create_table "program_titles", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.timestamp "start_time", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.timestamp "end_time", null: false
    t.integer "channel_id", null: false
    t.string "title", null: false
    t.text "desc", null: false
    t.integer "program_category_id", default: 0, null: false
    t.integer "next_counter", default: 1, null: false
    t.integer "weekdays", limit: 1, default: 0, null: false
    t.boolean "auto_next", default: true, null: false
    t.string "label_format", default: "", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["start_time", "channel_id"], name: "index_program_titles_on_start_time_and_channel_id", unique: true
  end

  create_table "programs", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.timestamp "start_time", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.timestamp "stop_time", null: false
    t.integer "channel_id", null: false
    t.string "title", null: false
    t.text "desc", null: false
    t.integer "event_id", default: 0, null: false
    t.integer "program_category_id", default: 0, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["start_time", "channel_id"], name: "index_programs_on_start_time_and_channel_id", unique: true
  end

  create_table "reservations", id: :bigint, unsigned: true, force: :cascade, options: "ENGINE=InnoDB DEFAULT CHARSET=utf8" do |t|
    t.timestamp "start_time", default: -> { "CURRENT_TIMESTAMP" }, null: false
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
    t.boolean "initialized", default: false, null: false
    t.integer "gr_tuner_count", default: 1, null: false
    t.integer "bs_tuner_count", default: 0, null: false
    t.integer "rest_gr_tuner_count", default: 1, null: false
    t.integer "rest_bs_tuner_count", default: 0, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["active"], name: "index_systems_on_active", unique: true
  end

end
