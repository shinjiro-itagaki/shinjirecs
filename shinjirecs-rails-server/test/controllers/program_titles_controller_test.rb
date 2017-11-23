require 'test_helper'

class ProgramTitlesControllerTest < ActionDispatch::IntegrationTest
  setup do
    @program_title = program_titles(:one)
  end

  test "should get index" do
    get program_titles_url, as: :json
    assert_response :success
  end

  test "should create program_title" do
    assert_difference('ProgramTitle.count') do
      post program_titles_url, params: { program_title: {  } }, as: :json
    end

    assert_response 201
  end

  test "should show program_title" do
    get program_title_url(@program_title), as: :json
    assert_response :success
  end

  test "should update program_title" do
    patch program_title_url(@program_title), params: { program_title: {  } }, as: :json
    assert_response 200
  end

  test "should destroy program_title" do
    assert_difference('ProgramTitle.count', -1) do
      delete program_title_url(@program_title), as: :json
    end

    assert_response 204
  end
end
