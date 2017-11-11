class ApplicationController < ActionController::API
  class << self
    def set_model(model) @model = model; end
    def model() @model; end
  end

  before_action :set_model
  before_action :set_record, only: [:show, :update, :destroy]
  before_action :system_check

  # GET /${record}s/params_info
  def params_info
    render_data @model.params_info
  end

  # GET /${record}s
  def index
    @records = @model.all
    render_data @records
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

  protected

  def render_data(obj=nil, options={})
    render( {json: {body: obj}}.merge(options) )
  end

  def set_model
    @model = self.class.model
  end

  # Use callbacks to share common setup or constraints between actions.
  def set_record
    @record = @model.find(params[:id])
  end

  # Only allow a trusted parameter "white list" through.
  def record_params
    params.fetch(:record, {})
  end

  def system_check
    if not System.get_instance then
    end
  end
end
