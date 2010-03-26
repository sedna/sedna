set(exp_SRC se_exp.c se_exp_common.c se_exp_export.c se_exp_import.c)
SET_SOURCES(exp_SRC 1)

# for now we'll link terminal with the static driver, since we don't have install
if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    if (MS_LINK_RUNTIME_STATIC)
        set(EXP_SEDNA_LIB sedna_mt)
    else (MS_LINK_RUNTIME_STATIC)
        set(EXP_SEDNA_LIB sedna_md)
    endif (MS_LINK_RUNTIME_STATIC)
else (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(EXP_SEDNA_LIB sedna_npic)
endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")

add_executable(se_exp ${exp_SRC})
set_target_properties(se_exp PROPERTIES LINKER_LANGUAGE "CXX")
target_link_libraries(se_exp common ${EXP_SEDNA_LIB} expat ${sysaux_LIB})
ConfigOutputDir(se_exp)

# install
install(TARGETS se_exp DESTINATION bin)
