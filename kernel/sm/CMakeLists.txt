# additional sub-libraries
set(lib_SUBDIRS llsm lm wu bufmgr)

# common sources for our executables
set(common_SRC sm_globals.cpp)

foreach (SUB ${lib_SUBDIRS})
    ADD_SUBFOLDER_SRC(common_SRC ${SUB})
endforeach (SUB ${lib_SUBDIRS})

# libraries for our executables
if (UNIX)
list(APPEND common_libs common expat ancillary ${sysaux_LIB})
else (UNIX)
list(APPEND common_libs common expat ${sysaux_LIB})
endif (UNIX)

# se_sm
set(sm_SRC sm.cpp sm_functions.cpp trmgr.cpp hb_utils.cpp cdb_utils.cpp ${common_SRC})
SET_SOURCES(sm_SRC 1)

add_executable(se_sm ${sm_SRC})
target_link_libraries(se_sm ${common_libs})

# place binaries in build/bin directory
set(sm_execs se_sm)
FinalizeBinaries(${sm_execs})

# set common dependencies
foreach(sm_exec ${sm_execs})
    add_dependencies(${sm_exec} generate_error_codes) # generate error files before any sm exec
    add_dependencies(${sm_exec} custom_expat) # find/build expat before any sm exec
endforeach(sm_exec)
    
# msvc-specific option
if (MSVC)
    set_target_properties(${sm_execs} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE")
endif (MSVC)

# install in bin
install(TARGETS ${sm_execs} RUNTIME DESTINATION bin)

