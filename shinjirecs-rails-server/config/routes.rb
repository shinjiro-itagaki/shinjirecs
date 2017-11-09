Rails.application.routes.draw do
  resources :program_categories
  resources :reservations
  resources :programs
  resources :channels
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
end
