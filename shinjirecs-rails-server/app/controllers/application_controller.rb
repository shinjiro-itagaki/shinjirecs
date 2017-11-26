class ApplicationController < ActionController::API
  class << self
    def set_model(model) @model = model; end
    def model() @model; end
    def set_parent_model(model,fkey)
      @parent_model = model
      @parent_fkey  = fkey
    end
    def parent_model() @parent_model; end
    def parent_fkey() @parent_fkey; end
  end

  before_action :set_models
  before_action :set_parent_record
  before_action :set_record, only: [:show, :update, :destroy]
  before_action :system_setup_check, except: [:params_info]

  rescue_from "ActiveRecord::RecordNotFound" do |e|
    # render status: 404, json: e
    # render_error 404, {id: e.id, model: e.model.name}
    # render_error 404, e
    render_error 404, {message: e.message, id: e.id, model: e.model}
  end

  # GET /${record}s/params_info
  def params_info
    render_data @model.params_info
  end

  # GET /${record}s
  def index
    @records = self.index_records_proxy.to_a
    render_data @records
  end

  def index_records_proxy
    @joins.default_all_proxy
  end

  # GET /${record}s/1
  def show
    render_data @record
  end

  # POST /${record}s
  def create
    @record = @model.new(record_params)

    if @record.save
      render_data @record, status: :created, location: @record
    else
      render_data @record.errors, status: :unprocessable_entity
    end
  end

  # PATCH/PUT /${record}s/1
  def update
    if @record.update(record_params)
      render_data @record
    else
      render_data @record.errors, status: :unprocessable_entity
    end
  end

  # DELETE /${record}s/1
  def destroy
    @record.destroy
  end

  def self.permitted_params
    []
  end

  protected

  def render_data(obj=nil, options={})
    render( {json: {header: options, body: obj}}.merge(options) )
  end

  def render_error(status, obj=nil)
    render status: status, json: obj
  end

  def set_models
    @model = self.class.model
    keys = @model.reflections.keys.map(&:to_sym)
    @joins = @model.includes(*keys)
    @parent_model = self.class.parent_model
    @parent_fkey  = self.class.parent_fkey
  end

  def set_parent_record
    if @parent_model && @parent_fkey then
      @parent_record = @parent_model.find(params[@parent_fkey])
    end
  end

  # Use callbacks to share common setup or constraints between actions.
  def set_record
    if @parent_model && @parent_fkey then
      @record = @joins.where(:id => params[:id], @parent_fkey => @parent_record.id).first
    else
      @record = @joins.find(params[:id])
    end
  end

  # Only allow a trusted parameter "white list" through.
  def record_params
    params.require(:record).permit(self.class.permitted_params)
  end

  def system_setup_check
    ins = System.instance
    if not (ins && ins.setup?) then
      render_data nil, system: ins, setup: false
    end
  end
end
