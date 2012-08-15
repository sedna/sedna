enable_testing()
include_directories(../kernel)
include_directories(gtest-1.6.0/include/)

add_subdirectory(gtest-1.6.0)

add_executable(test_options xml_options_test.cpp)

add_dependencies(u generate_error_codes gen_error_codes)
add_dependencies(test_options u common gtest auxiliary generate_error_codes gen_error_codes errdbg)

target_link_libraries(test_options u common gtest ${sysaux_LIB})

add_executable(event_log_test event_log_test.cpp ../kernel/common/errdbg/event_log.c 
               ../kernel/common/errdbg/errors.c)

add_dependencies(event_log_test u gtest common)
target_link_libraries(event_log_test u gtest common ${sysaux_LIB})

add_executable(global_names_test global_names_test.cpp)
add_dependencies(global_names_test generate_error_codes u common gtest)
target_link_libraries(global_names_test u common gtest ${sysaux_LIB})

add_executable(protocol_test protocol_test.cpp)
add_dependencies(protocol_test common u)
target_link_libraries(protocol_test common u)


add_test(NAME event_log_test COMMAND event_log_test)

add_test(NAME test_options COMMAND test_options)

add_test(NAME global_names_test COMMAND global_names_test)



set(TEST_SET event_log_test test_options global_names_test)

add_custom_target(testing COMMAND ${CMAKE_CTEST_COMMAND}
                  DEPENDS ${TEST_SET})