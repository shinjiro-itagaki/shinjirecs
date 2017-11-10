class ProgramTitlesController < ApplicationController
  before_action :set_program_title, only: [:show, :update, :destroy]

  # GET /program_titles
  def index
    @program_titles = ProgramTitle.all

    render json: @program_titles
  end

  # GET /program_titles/1
  def show
    render json: @program_title
  end

  # POST /program_titles
  def create
    @program_title = ProgramTitle.new(program_title_params)

    if @program_title.save
      render json: @program_title, status: :created, location: @program_title
    else
      render json: @program_title.errors, status: :unprocessable_entity
    end
  end

  # PATCH/PUT /program_titles/1
  def update
    if @program_title.update(program_title_params)
      render json: @program_title
    else
      render json: @program_title.errors, status: :unprocessable_entity
    end
  end

  # DELETE /program_titles/1
  def destroy
    @program_title.destroy
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_program_title
      @program_title = ProgramTitle.find(params[:id])
    end

    # Only allow a trusted parameter "white list" through.
    def program_title_params
      params.fetch(:program_title, {})
    end
end
