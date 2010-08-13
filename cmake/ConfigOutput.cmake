# we need version comparison here
include(CompareVersions)

# This macro defines output directory to be <binary_tree>/<config_type>/bin for MSVC IDE. This is usually needed to
# to obtain install-like binary tree, so that Debug configuration goes to <build_tree>/Debug
# Works natively for CMake >= 2.8.1 though...
macro (ConfigOutputDir ...)
    COMPARE_VERSION_STRINGS(${CMAKE_VERSION} "2.8.1" ver_res)
    if (MSVC_IDE AND NOT ver_res EQUAL -1)
        foreach(conf ${CMAKE_CONFIGURATION_TYPES})
            string(TOUPPER ${conf} conf_u)
            set_target_properties(${ARGV} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_${conf_u} ${CMAKE_BINARY_DIR}/${conf}/bin)
        endforeach(conf)
    else (MSVC_IDE AND NOT ver_res EQUAL -1)
        set_target_properties(${ARGV} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
    endif (MSVC_IDE AND NOT ver_res EQUAL -1)
endmacro (ConfigOutputDir ...)

# This macro makes somewhat similar things to the previous one, but for general files
macro (ConfigOutputTree)
    COMPARE_VERSION_STRINGS(${CMAKE_VERSION} "2.8.1" ver_res)
    if (MSVC_IDE AND NOT ver_res EQUAL -1)
        foreach(conf ${CMAKE_CONFIGURATION_TYPES})
            configure_file(etc/sednaconf.xml.sample ${CMAKE_BINARY_DIR}/${conf}/etc/sednaconf.xml.sample COPYONLY)
            configure_file(share/sedna_auth_md.xml ${CMAKE_BINARY_DIR}/${conf}/share/sedna_auth_md.xml COPYONLY)
            configure_file(driver/c/libsedna.h ${CMAKE_BINARY_DIR}/driver/c/${conf}/libsedna.h COPYONLY)
            configure_file(driver/c/sp_defs.h ${CMAKE_BINARY_DIR}/driver/c/${conf}/sp_defs.h COPYONLY)
        endforeach(conf)
    else (MSVC_IDE AND NOT ver_res EQUAL -1)
        configure_file(etc/sednaconf.xml.sample ${CMAKE_BINARY_DIR}/etc/sednaconf.xml.sample COPYONLY)
        configure_file(share/sedna_auth_md.xml ${CMAKE_BINARY_DIR}/share/sedna_auth_md.xml COPYONLY)
    endif (MSVC_IDE AND NOT ver_res EQUAL -1)
endmacro (ConfigOutputTree)

# This macro performs some binaries finalization:
#   1) sets VERSION properly
#   2) sets binary output dir appropriately
macro(FinalizeBinaries ...)
    ConfigOutputDir(${ARGV})

    # set version only for windows since on linux cmake makes strange links
    if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        set_target_properties(${ARGV} PROPERTIES VERSION ${SEDNA_BUILD_VERSION})
    endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
endmacro(FinalizeBinaries ...)
