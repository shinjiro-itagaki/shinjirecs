require 'timeout'
require 'open3'
require 'tempfile'

class ChannelsController < ApplicationController
  class ScanThread < Thread
    attr_accessor :stop
  end

  set_model Channel
  def self.permitted_params
    [:number,:area_id,:ctype,:display_name,:display_name_locked,:order]
  end

  def scan
    namespace = "channels_controller#scan"
    res = nil
    if params["stop"] then
      res = self.class.stop_thread namespace
    else
      res = self.class.run_thread(namespace, params[:gr], params[:bs], params[:timeout]) do |gr,bs,timeout|
        Channel.scan params[:gr], params[:bs], params[:timeout]
      end
    end
    render_data result: res
  end

  protected
  def system_setup_check
    if not System.instance
      render_data nil, system: System.instance, setup: false
    end
  end
end
