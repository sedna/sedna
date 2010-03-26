# common libraries for our executables
set(common_libs common expat ${sysaux_LIB})

# se_hb
set(hb_SRC hot_backup.cpp hb_aux.cpp hb_main.cpp hb_files.cpp)

SET_SOURCES(hb_SRC 1)
add_executable(se_hb ${hb_SRC})
target_link_libraries(se_hb ${common_libs})
ConfigOutputDir(se_hb)

# install
install(TARGETS se_hb RUNTIME DESTINATION bin)
