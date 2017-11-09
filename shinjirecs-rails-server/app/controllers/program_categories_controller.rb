class ProgramCategoriesController < ApplicationController
  before_action :set_program_category, only: [:show, :update, :destroy]

  # GET /program_categories
  def index
    @program_categories = ProgramCategory.all

    render json: @program_categories
  end

  # GET /program_categories/1
  def show
    render json: @program_category
  end

  # POST /program_categories
  def create
    @program_category = ProgramCategory.new(program_category_params)

    if @program_category.save
      render json: @program_category, status: :created, location: @program_category
    else
      render json: @program_category.errors, status: :unprocessable_entity
    end
  end

  # PATCH/PUT /program_categories/1
  def update
    if @program_category.update(program_category_params)
      render json: @program_category
    else
      render json: @program_category.errors, status: :unprocessable_entity
    end
  end

  # DELETE /program_categories/1
  def destroy
    @program_category.destroy
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_program_category
      @program_category = ProgramCategory.find(params[:id])
    end

    # Only allow a trusted parameter "white list" through.
    def program_category_params
      params.fetch(:program_category, {})
    end
end
