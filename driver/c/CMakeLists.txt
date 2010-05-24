# sources from common library
set(common_dir ${CMAKE_SOURCE_DIR}/kernel/common)
set(common_drv_SRC ${common_dir}/sp.c ${common_dir}/u/uhdd.c ${common_dir}/u/usocket.c ${common_dir}/u/uutils.c ${common_dir}/u/usecurity.c ${common_dir}/errdbg/d_printf.c
    ${CMAKE_BINARY_DIR}/kernel/common/errdbg/error_codes.c ${common_dir}/u/u.c)

# error_codes.c is a generated file
set_source_files_properties(${CMAKE_BINARY_DIR}/kernel/common/errdbg/error_codes.c PROPERTIES GENERATED TRUE)

# driver sources
set(cdriver_SRC libsedna.c ${common_drv_SRC} libsedna.h)
source_group("" FILES libsedna.h libsedna.c ${common_drv_SRC})

# add definition to turn off event log
add_definitions(-DSE_NO_EVENT_LOG)

# static sedna driver

# for Windows we create two static libs: one with /MT and the other with /MD
# on Linux-like systems we create two libs: one with pic and the other with non-pic code
if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    add_library(sedna_mt STATIC ${cdriver_SRC})
    add_dependencies(sedna_mt generate_error_codes)
    set_target_properties(sedna_mt PROPERTIES OUTPUT_NAME libsednamt)

    add_library(sedna_md STATIC ${cdriver_SRC})
    add_dependencies(sedna_md generate_error_codes)
    set_target_properties(sedna_md PROPERTIES OUTPUT_NAME libsednamd)

    # make sure our mt and md libraries are linked with correct runtime
    if (MS_LINK_RUNTIME_STATIC AND MSVC)
        set_target_properties(sedna_md PROPERTIES COMPILE_FLAGS "/MD")
    elseif (MSVC) # dynamic linking in progress
        set_target_properties(sedna_mt PROPERTIES COMPILE_FLAGS "/MT")
    endif (MS_LINK_RUNTIME_STATIC AND MSVC)

    # static libs
    set(sedna_st_libs sedna_md sedna_mt)
else (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
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

    # static libs
    set(sedna_st_libs sedna_pic sedna_npic)
endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")

# dynamic sedna library
if (MSVC)
    list(APPEND cdriver_SRC sednamt.def) # add defifnition file
    source_group("" FILES sednamt.def)
endif (MSVC)

add_library(sedna_dyn SHARED ${cdriver_SRC})
add_dependencies(sedna_dyn generate_error_codes)
set_target_properties(sedna_dyn PROPERTIES OUTPUT_NAME sedna)
set_target_properties(sedna_dyn PROPERTIES CLEAN_DIRECT_OUTPUT 1)

# build dylib as a single module on Darwin (more efficient and allows to get rid of problems with common symbols)
if (APPLE)
    set_target_properties(sedna_dyn PROPERTIES LINK_FLAGS -single_module)
endif (APPLE)

# make sure our dll is linked with static runtime
if (MSVC AND NOT MS_LINK_RUNTIME_STATIC)
    set_target_properties(sedna_dyn PROPERTIES COMPILE_FLAGS "/MT")
endif (MSVC AND NOT MS_LINK_RUNTIME_STATIC)

# set proper build and library version
set_target_properties(sedna_dyn PROPERTIES SOVERSION ${SEDNA_PROTOCOL_VERSION})

# custom target to build all sedna libraries
add_custom_target(sedna_library ALL
                  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/libsedna.h ${CMAKE_CURRENT_BINARY_DIR}
                  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/sp_defs.h ${CMAKE_CURRENT_BINARY_DIR}
                  DEPENDS ${sedna_st_libs} sedna_dyn
                 )

# install
install(TARGETS ${sedna_st_libs} DESTINATION driver/c)
install(TARGETS sedna_dyn DESTINATION driver/c)
install(FILES libsedna.h sp_defs.h DESTINATION driver/c)
