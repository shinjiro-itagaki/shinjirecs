class CreateChannels < ActiveRecord::Migration[5.1]
  def change
    create_table :channels, unsigned: true, column_options: {ctype: "CHECK(ctype IN ('gr','bs'))"} do |t|
      t.integer "number"        ,null: false
      t.string  "ctype"         ,null: false, default: "gr"
      t.string  "display_name"  ,null: false
      t.integer "order"         ,null: false, default: 0
      t.timestamps               null: false
    end
  end
end
