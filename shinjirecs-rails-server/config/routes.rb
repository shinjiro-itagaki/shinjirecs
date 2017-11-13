Rails.application.routes.draw do
  root to: "systems#root"

  [
    :program_titles,
    :systems,
    :areas,
    :epg_program_categories,
    :reservations,
    :epg_programs,
    :channels
  ].each do |sym|
    resources sym do
      collection do
        get :params_info
      end
    end
  end

  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
end
