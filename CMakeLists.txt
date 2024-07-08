cmake_minimum_required(VERSION 3.15)
include(cmake/warnings.cmake)

project(slo CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
add_library(slo_typelist INTERFACE)

set_property(TARGET slo_typelist PROPERTY CXX_STANDARD 23)

target_include_directories(slo_typelist INTERFACE 
    $<INSTALL_INTERFACE:include>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>)


option(BUILD_TESTING "Enable tests" ON)

if(NOT BUILD_TESTING STREQUAL OFF)
  message(STATUS "Building unit tests")

  enable_testing()
  add_executable(slo_typelist_test "")
  
  enable_warnings(slo_typelist_test)

  find_package(GTest REQUIRED)
  target_link_libraries(slo_typelist_test PRIVATE slo_typelist)
  target_link_libraries(slo_typelist_test PRIVATE GTest::gtest GTest::gtest_main GTest::gmock)

  include(GoogleTest)
  gtest_discover_tests(slo_typelist_test)
  
  add_subdirectory(test)
endif()

set_target_properties(slo_typelist PROPERTIES PUBLIC_HEADER "include/slo")
install(DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/include/
        DESTINATION include)
