include_directories(../kernel)
add_executable(test_options Options.cpp)
target_link_libraries(test_options common)
add_dependencies(test_options generate_error_codes)
add_dependencies(test_options common)

