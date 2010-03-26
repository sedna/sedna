# common libraries for our executables
set(common_libs common expat ${sysaux_LIB})

# se_gov
set(gov_SRC gov_globals.cpp gov_table.cpp listener.cpp gov_functions.cpp gov.cpp config_utils.cpp hb_funcs.cpp hb_files.cpp)
SET_SOURCES(gov_SRC 1)

add_executable(se_gov ${gov_SRC})
target_link_libraries(se_gov ${common_libs})

# se_stop
set(stop_SRC stop_server.cpp)
SET_SOURCES(stop_SRC 1)

add_executable(se_stop ${stop_SRC})
target_link_libraries(se_stop ${common_libs})

# se_rc
set(rc_SRC rc.cpp)
SET_SOURCES(rc_SRC 1)

add_executable(se_rc ${rc_SRC})
target_link_libraries(se_rc ${common_libs})

# place binaries in build/bin directory
set(gov_execs se_gov se_stop se_rc)
ConfigOutputDir(${gov_execs})

# install it in bin
install(TARGETS ${gov_execs} RUNTIME DESTINATION bin)