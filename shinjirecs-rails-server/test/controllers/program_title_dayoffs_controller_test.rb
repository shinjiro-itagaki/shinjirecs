require 'test_helper'

class ProgramTitleDayoffsControllerTest < ActionDispatch::IntegrationTest
  setup do
    @program_title_dayoff = program_title_dayoffs(:one)
  end

  test "should get index" do
    get program_title_dayoffs_url, as: :json
    assert_response :success
  end

  test "should create program_title_dayoff" do
    assert_difference('ProgramTitleDayoff.count') do
      post program_title_dayoffs_url, params: { program_title_dayoff: {  } }, as: :json
    end

    assert_response 201
  end

  test "should show program_title_dayoff" do
    get program_title_dayoff_url(@program_title_dayoff), as: :json
    assert_response :success
  end

  test "should update program_title_dayoff" do
    patch program_title_dayoff_url(@program_title_dayoff), params: { program_title_dayoff: {  } }, as: :json
    assert_response 200
  end

  test "should destroy program_title_dayoff" do
    assert_difference('ProgramTitleDayoff.count', -1) do
      delete program_title_dayoff_url(@program_title_dayoff), as: :json
    end

    assert_response 204
  end
end
