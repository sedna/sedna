include_directories(../kernel)

add_executable(test_options Options.cpp)
target_link_libraries(test_options common)
add_dependencies(test_options generate_error_codes)
add_dependencies(test_options common)

# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
# set(u_SRC ../kernel/u/u.c ../kernel/u/uevent.c ../kernel/u/ugnames.cpp ../kernel/u/uhdd.c ../kernel/u/ushm.c ../kernel/u/ummap.c ../kernel/u/umutex.c ../kernel/u/uprocess.c ../kernel/u/usecurity.c ../kernel/u/usem.c ../kernel/u/usocket.c ../kernel/u/usystem.c ../kernel/u/uthread.c ../kernel/u/utime.c ../kernel/u/uutils.c)
# SET_SOURCES(u_SRC 1)

add_executable(event_log_test event_log_test.cpp ../kernel/common/errdbg/event_log.c 
               ../kernel/common/errdbg/errors.c)

add_dependencies(event_log_test u)
target_link_libraries(event_log_test u)
target_link_libraries(event_log_test ${sysaux_LIB})

