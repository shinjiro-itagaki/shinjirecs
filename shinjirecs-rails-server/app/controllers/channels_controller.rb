require 'timeout'
require 'open3'
require 'tempfile'

class ChannelsController < ApplicationController
  class ScanThread < Thread
    attr_accessor :cancel
  end

  set_model Channel
  def self.permitted_params
    [:number,:area_id,:ctype,:display_name,:display_name_locked,:order]
  end

  @@scan_thread=nil

  def scan
    if @@scan_thread then
      if params["cancel"] then
        @@scan_thread.cancel = true
        @@scan_thread.exit
        @@scan_thread = nil
        render_data canceled: true
      else
        render_data working: true
      end
    end

    @@scan_thread ||= ScanThread.start do
      ActiveRecord::Base.connection_pool.with_connection do
        Channel.scan params[:gr], params[:bs], params[:timeout] do
          if (th = Thread.current).kind_of? ScanThread then
            th.canceled
          else
            false
          end
        end
      end
    end
    render_data start: true
  end

  protected
  def system_setup_check
    if not System.instance
      render_data nil, system: System.instance, setup: false
    end
  end
end
