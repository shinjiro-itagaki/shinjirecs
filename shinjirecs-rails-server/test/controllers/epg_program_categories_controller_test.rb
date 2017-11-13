require 'test_helper'

class EpgProgramCategoriesControllerTest < ActionDispatch::IntegrationTest
  setup do
    @epg_program_category = program_categories(:one)
  end

  test "should get index" do
    get program_categories_url, as: :json
    assert_response :success
  end

  test "should create epg_program_category" do
    assert_difference('ProgramCategory.count') do
      post program_categories_url, params: { epg_program_category: {  } }, as: :json
    end

    assert_response 201
  end

  test "should show epg_program_category" do
    get epg_program_category_url(@epg_program_category), as: :json
    assert_response :success
  end

  test "should update epg_program_category" do
    patch epg_program_category_url(@epg_program_category), params: { epg_program_category: {  } }, as: :json
    assert_response 200
  end

  test "should destroy epg_program_category" do
    assert_difference('ProgramCategory.count', -1) do
      delete epg_program_category_url(@epg_program_category), as: :json
    end

    assert_response 204
  end
end
