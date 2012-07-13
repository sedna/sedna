include_directories(../kernel)
include_directories(gtest-1.6.0/include/)

add_subdirectory(gtest-1.6.0)

add_executable(test_options xml_options_test.cpp)

add_dependencies(test_options generate_error_codes gtest auxiliary common)

target_link_libraries(test_options u common gtest ${sysaux_LIB})

add_executable(event_log_test event_log_test.cpp ../kernel/common/errdbg/event_log.c 
               ../kernel/common/errdbg/errors.c)

add_dependencies(event_log_test u gtest common)
target_link_libraries(event_log_test u gtest common ${sysaux_LIB})

add_executable(global_names_test global_names_test.cpp)
add_dependencies(global_names_test generate_error_codes u common gtest)
target_link_libraries(global_names_test u common gtest ${sysaux_LIB})

