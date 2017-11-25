Rails.application.routes.draw do
  # root to: "systems#all"
  get "systems/all", to: "systems#all"

  [ :systems,
    :areas,
    :epg_program_categories,
    :reservations,
    :epg_programs,
    :channels,
    :epgdump_schedules
  ].each do |sym|
    resources sym do
      collection do
        get :params_info
      end
    end
  end

  resources :program_titles do
    [ :program_title_dayoffs,
      :program_title_terms].each do |sym|
      resources sym
    end

    collection do
      get :params_info
    end
  end

  [ :program_title_dayoffs,
    :program_title_terms].each do |sym|
    namespace sym do
      get :params_info
    end
  end

  post "channels/scan", to: "channels#scan" #
  post "epg_programs/epgdump", to: "epg_programs#epgdump" #

  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
end
