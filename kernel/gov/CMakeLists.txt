# common libraries for our executables
if (UNIX)
set(common_libs common expat ancillary ${sysaux_LIB})
else (UNIX)
set(common_libs common expat ${sysaux_LIB})
endif (UNIX)

# se_gov
set(gov_SRC gov_globals.cpp gov_table.cpp listener.cpp gov_functions.cpp gov.cpp config_utils.cpp hb_funcs.cpp hb_files.cpp clients.cpp cpool.cpp processes.cpp processes.h)
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

# test_cdb
set(test_cdb_SRC test_cdb.cpp)
SET_SOURCES(test_cdb_SRC 1)

add_executable(test_cdb ${test_cdb_SRC})
target_link_libraries(test_cdb ${common_libs})

#test_smsd
set(test_smsd_SRC test_smsd.cpp)
SET_SOURCES(test_smsd_SRC 1)

add_executable(test_smsd ${test_smsd_SRC})
target_link_libraries(test_smsd ${common_libs})


#test_stop
set(test_stop_SRC test_stop.cpp)
SET_SOURCES(test_stop_SRC 1)

add_executable(test_stop ${test_stop_SRC})
target_link_libraries(test_stop ${common_libs})

# place binaries in build/bin directory
set(gov_execs se_gov se_stop se_rc test_cdb test_stop test_smsd)
FinalizeBinaries(${gov_execs})

# install it in bin
install(TARGETS ${gov_execs} RUNTIME DESTINATION bin)