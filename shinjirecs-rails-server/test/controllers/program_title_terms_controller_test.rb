require 'test_helper'

class ProgramTitleTermsControllerTest < ActionDispatch::IntegrationTest
  setup do
    @program_title_term = program_title_terms(:one)
  end

  test "should get index" do
    get program_title_terms_url, as: :json
    assert_response :success
  end

  test "should create program_title_term" do
    assert_difference('ProgramTitleTerm.count') do
      post program_title_terms_url, params: { program_title_term: {  } }, as: :json
    end

    assert_response 201
  end

  test "should show program_title_term" do
    get program_title_term_url(@program_title_term), as: :json
    assert_response :success
  end

  test "should update program_title_term" do
    patch program_title_term_url(@program_title_term), params: { program_title_term: {  } }, as: :json
    assert_response 200
  end

  test "should destroy program_title_term" do
    assert_difference('ProgramTitleTerm.count', -1) do
      delete program_title_term_url(@program_title_term), as: :json
    end

    assert_response 204
  end
end
