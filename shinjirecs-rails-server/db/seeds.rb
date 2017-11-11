# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

area = Area.find_or_create_by!(id: 0, label: 'デフォルト')

System.create area: area , active: true  if not System.instance.exists?
System.create area: area , active: false if not System.dummy.exists?
ProgramCategory.find_or_create_by!(id: 0, label: 'その他')
