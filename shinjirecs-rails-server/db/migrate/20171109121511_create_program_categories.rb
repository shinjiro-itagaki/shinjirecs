class CreateProgramCategories < ActiveRecord::Migration[5.1]
  def change
    create_table :program_categories do |t|
      t.string     "label" , null: false
      t.timestamps           null: false
    end
  end
end
