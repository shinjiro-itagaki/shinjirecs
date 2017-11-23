class AreasController < ApplicationController
  set_model Area
  def self.permitted_params
    [:label,:channels_checked]
  end
end
