# common libraries for our executables
if (UNIX)
set(common_libs common expat ancillary ${sysaux_LIB})
else (UNIX)
set(common_libs common expat ${sysaux_LIB})
endif (UNIX)

# se_gov
set(gov_SRC clients.cpp cpool.cpp gov.cpp gov_functions.cpp gov_globals.cpp hb_files.cpp hb_funcs.cpp process_manager.cpp)
SET_SOURCES(gov_SRC 1)

add_executable(se_gov ${gov_SRC})
target_link_libraries(se_gov ${common_libs})

# place binaries in build/bin directory
set(gov_execs se_gov)
FinalizeBinaries(${gov_execs})

# install it in bin
install(TARGETS ${gov_execs} RUNTIME DESTINATION bin)