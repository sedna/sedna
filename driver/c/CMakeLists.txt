# sources from common library
set(sedna_dir ${CMAKE_SOURCE_DIR}/kernel)

set(common_drv_SRC
    ${sedna_dir}/u/u.c
    ${sedna_dir}/u/uhdd.c
    ${sedna_dir}/u/usocket.c
    ${sedna_dir}/u/uutils.c
    ${sedna_dir}/u/usecurity.c
    ${sedna_dir}/common/protocol/sp.c
    ${sedna_dir}/common/errdbg/d_printf.c
    ${sedna_dir}/common/errdbg/error_codes.c)

# error_codes.c is a generated file
set_source_files_properties(${sedna_dir}/common/errdbg/error_codes.c
                            PROPERTIES GENERATED TRUE)

# driver sources
set(cdriver_SRC libsedna.c ${common_drv_SRC} libsedna.h)
source_group("" FILES ${cdriver_SRC})

# add definition to turn off event log
add_definitions(-DSE_NO_EVENT_LOG)

# static sedna driver

# for MSVC we create two static libs: one with /MT and the other with /MD
#                and two dynamic libs: one with /MT and the other with /MD
# for others we create two libs: one with pic and the other with non-pic code
if (MSVC)
    # add definition file for MSVC
    list(APPEND cdriver_SRC sednamt.def)
    source_group("" FILES sednamt.def)

    add_library(sedna_mt STATIC ${cdriver_SRC})
    add_dependencies(sedna_mt generate_error_codes)
    set_target_properties(sedna_mt PROPERTIES OUTPUT_NAME libsednamt)

    add_library(sedna_md STATIC ${cdriver_SRC})
    add_dependencies(sedna_md generate_error_codes)
    set_target_properties(sedna_md PROPERTIES OUTPUT_NAME libsednamd)

    add_library(sedna_dyn_md SHARED ${cdriver_SRC})
    add_dependencies(sedna_dyn_md generate_error_codes)
    set_target_properties(sedna_dyn_md PROPERTIES OUTPUT_NAME sednamd)

    add_library(sedna_dyn_mt SHARED ${cdriver_SRC})
    add_dependencies(sedna_dyn_mt generate_error_codes)
    set_target_properties(sedna_dyn_mt PROPERTIES OUTPUT_NAME sednamt)

    # make sure our mt and md libraries are linked with correct runtime
    # there will be warnings from cl on /MD(/MT) override but that's ok
    if (MS_LINK_RUNTIME_STATIC)
        set(MD_MT_REPLACE_FLAG "/MD")
    else (MS_LINK_RUNTIME_STATIC)
        set(MD_MT_REPLACE_FLAG "/MT")
    endif (MS_LINK_RUNTIME_STATIC)
    if (NOT CMAKE_CONFIGURATION_TYPES AND "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
        set(MD_MT_REPLACE_FLAG "${MD_MT_REPLACE_FLAG}d")
    endif (NOT CMAKE_CONFIGURATION_TYPES AND "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")

    if (MS_LINK_RUNTIME_STATIC)
        set_target_properties(sedna_md sedna_dyn_md PROPERTIES
                COMPILE_FLAGS ${MD_MT_REPLACE_FLAG})
    else (MS_LINK_RUNTIME_STATIC) # dynamic linking in progress
        set_target_properties(sedna_mt sedna_dyn_mt PROPERTIES
                COMPILE_FLAGS ${MD_MT_REPLACE_FLAG})
    endif (MS_LINK_RUNTIME_STATIC)

    # static libs
    set(sedna_st_libs sedna_md sedna_mt)
    # dynamic libs
    set(sedna_dyn_libs sedna_dyn_md sedna_dyn_mt)
else (MSVC)
    add_library(sedna_npic STATIC ${cdriver_SRC})
    add_dependencies(sedna_npic generate_error_codes)
    set_target_properties(sedna_npic PROPERTIES OUTPUT_NAME sedna)
    set_target_properties(sedna_npic PROPERTIES CLEAN_DIRECT_OUTPUT 1)

    add_library(sedna_pic STATIC ${cdriver_SRC})
    add_dependencies(sedna_pic generate_error_codes)
    set_target_properties(sedna_pic PROPERTIES COMPILE_FLAGS -fPIC)
    # add PIC definition to FreeBSD (do we really need this?)
    if ("${CMAKE_SYSTEM_NAME}" MATCHES "FreeBSD")
        set_target_properties(sedna_pic PROPERTIES COMPILE_DEFINITIONS PIC)
    endif ("${CMAKE_SYSTEM_NAME}" MATCHES "FreeBSD")
    set_target_properties(sedna_pic PROPERTIES OUTPUT_NAME sedna_pic)

    add_library(sedna_dyn SHARED ${cdriver_SRC})
    add_dependencies(sedna_dyn generate_error_codes)
    set_target_properties(sedna_dyn PROPERTIES OUTPUT_NAME sedna)
    set_target_properties(sedna_dyn PROPERTIES CLEAN_DIRECT_OUTPUT 1)

    # build dylib as a single module on Darwin (more efficient and allows to
    # get rid of problems with common symbols)
    if (APPLE)
        set_target_properties(sedna_dyn PROPERTIES LINK_FLAGS -single_module)
    endif (APPLE)

    # static libs
    set(sedna_st_libs sedna_pic sedna_npic)
    # dynamic libs
    set(sedna_dyn_libs sedna_dyn)
endif (MSVC)

# set proper build and library version
set_target_properties(${sedna_dyn_libs} PROPERTIES SOVERSION ${SEDNA_PROTOCOL_VERSION})

# custom target to build all sedna libraries
add_custom_target(sedna_library ALL
                  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/libsedna.h ${CMAKE_CURRENT_BINARY_DIR}
                  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/sp_defs.h ${CMAKE_CURRENT_BINARY_DIR}
                  DEPENDS ${sedna_st_libs} ${sedna_dyn_libs}
                 )

# install
install(TARGETS ${sedna_st_libs} DESTINATION driver/c)
install(TARGETS ${sedna_dyn_libs} DESTINATION driver/c)
install(FILES libsedna.h sp_defs.h DESTINATION driver/c)
