# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

area = (if Area.exists?
          Area.first
        else
          Area.new(label: 'デフォルト').save
        end)

System.create area: area , active: true  if not System.where_instance.exists?
System.create area: area , active: false if not System.where_dummy.exists?
categ = EpgProgramCategory.find_or_create_by!(id: 0, label_ja: "その他", label_en: "others")
EpgProgramMediumCategory.find_or_create_by!(id: 0, label_ja: "その他", label_en: "others", parent_id: categ.id)
