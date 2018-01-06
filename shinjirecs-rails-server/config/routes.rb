Rails.application.routes.draw do
  # root to: "systems#all"
  get "systems/all", to: "systems#all"
  post   "systems/add_epgdump_schedule",  to: "systems#add_epgdump_schedule"
  delete "systems/del_epgdump_schedules", to: "systems#del_epgdump_schedules"
  get  "epg_programs/search", to: "epg_programs#search" #

  post "channels/scan", to: "channels#scan" #
  get  "epg_programs/epgdump", to: "epg_programs#epgdump" #
  post "epg_programs/epgdump", to: "epg_programs#epgdump" #
  post "epg_programs/:id/new_reservation", to: "epg_programs#new_reservation" #

  [ :systems,
    :areas,
    :epg_program_categories,
    :reservations,
    :epg_programs,
    :channels,
    :program_series
  ].each do |sym|
    resources sym do
      collection do
        get :params_info
      end
    end
  end

  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
end
