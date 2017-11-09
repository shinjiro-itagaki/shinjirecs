require 'test_helper'

class ProgramCategoriesControllerTest < ActionDispatch::IntegrationTest
  setup do
    @program_category = program_categories(:one)
  end

  test "should get index" do
    get program_categories_url, as: :json
    assert_response :success
  end

  test "should create program_category" do
    assert_difference('ProgramCategory.count') do
      post program_categories_url, params: { program_category: {  } }, as: :json
    end

    assert_response 201
  end

  test "should show program_category" do
    get program_category_url(@program_category), as: :json
    assert_response :success
  end

  test "should update program_category" do
    patch program_category_url(@program_category), params: { program_category: {  } }, as: :json
    assert_response 200
  end

  test "should destroy program_category" do
    assert_difference('ProgramCategory.count', -1) do
      delete program_category_url(@program_category), as: :json
    end

    assert_response 204
  end
end
