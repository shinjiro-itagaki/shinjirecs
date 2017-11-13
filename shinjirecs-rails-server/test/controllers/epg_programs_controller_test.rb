require 'test_helper'

class EpgProgramsControllerTest < ActionDispatch::IntegrationTest
  setup do
    @epg_program = epg_programs(:one)
  end

  test "should get index" do
    get epg_programs_url, as: :json
    assert_response :success
  end

  test "should create epg_program" do
    assert_difference('Epg_Program.count') do
      post epg_programs_url, params: { epg_program: {  } }, as: :json
    end

    assert_response 201
  end

  test "should show epg_program" do
    get epg_program_url(@epg_program), as: :json
    assert_response :success
  end

  test "should update epg_program" do
    patch epg_program_url(@epg_program), params: { epg_program: {  } }, as: :json
    assert_response 200
  end

  test "should destroy epg_program" do
    assert_difference('EpgProgram.count', -1) do
      delete epg_program_url(@epg_program), as: :json
    end

    assert_response 204
  end
end
