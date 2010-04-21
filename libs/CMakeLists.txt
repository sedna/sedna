# pcre library
add_subdirectory(pcre)

# try to find gmake, and then fall back to make
if (UNIX)
    find_program(GMAKE_BINARY gmake)
    if (NOT GMAKE_BINARY)
        message(STATUS "Cannot find gmake -- will try to use make to build custom libraries")
        set(GMAKE_BINARY make)
    endif (NOT GMAKE_BINARY)
endif (UNIX)

# expat "external" library
# we use internal cached variables to determine directories for -I and -L: 1) look for cache 2) then try to describe builduing expat from scratch
set(EXPAT_VER "1.95.8")
message(STATUS "Searching for expat binaries, version ${EXPAT_VER}")
if (EXISTS ${CMAKE_SOURCE_DIR}/libs/bin/${CMAKE_SYSTEM_NAME}/expat) # look for binary cache
    message(STATUS "Binary cache for ${CMAKE_SYSTEM_NAME} is found. Taking expat from -- ${CMAKE_SOURCE_DIR}/libs/bin/${CMAKE_SYSTEM_NAME}/expat")
    set(CUSTOM_EXPAT_INCLUDE ${CMAKE_SOURCE_DIR}/libs/bin/${CMAKE_SYSTEM_NAME}/expat CACHE INTERNAL "Internal expat include path")
    set(CUSTOM_EXPAT_LIB ${CMAKE_SOURCE_DIR}/libs/bin/${CMAKE_SYSTEM_NAME}/expat CACHE INTERNAL "Internal expat library path")
    add_custom_target(custom_expat)
else (EXISTS ${CMAKE_SOURCE_DIR}/libs/${CMAKE_SYSTEM_NAME}/expat) # extract expat sources from .tar.gz and build it from scratch
    message(STATUS "Binary cache for expat on ${CMAKE_SYSTEM_NAME} not found. Will build expat from scratch when building the project...")

    set(EXPAT_TAR_DIR ${CMAKE_BINARY_DIR}/libs/expat-${EXPAT_VER})
    set(expat_files ${EXPAT_TAR_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}expat${CMAKE_STATIC_LIBRARY_SUFFIX} ${EXPAT_TAR_DIR}/lib/expat.h ${EXPAT_TAR_DIR}/lib/expat_external.h)

    set(CUSTOM_EXPAT_INCLUDE ${EXPAT_TAR_DIR}/lib CACHE INTERNAL "Internal expat include path")
    set(CUSTOM_EXPAT_LIB ${EXPAT_TAR_DIR}/lib CACHE INTERNAL "Internal expat library path")

    if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        find_program(NMAKE_BINARY nmake)
        if (NOT NMAKE_BINARY)
            message(FATAL_ERROR "Cannot find nmake to build expat library -- aborting")
        endif (NOT NMAKE_BINARY)
        set(expat_config_command cmd /C echo Expat configure...)
        set(expat_make_command ${NMAKE_BINARY} /f ${CMAKE_SOURCE_DIR}/libs/expat_static.mak)
    set(expat_build_dir ${EXPAT_TAR_DIR}/lib)
    else (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        set(expat_config_command sh configure --enable-shared=no --prefix=${EXPAT_TAR_DIR})
        set(expat_make_command ${GMAKE_BINARY} install)
    set(expat_build_dir ${EXPAT_TAR_DIR})
    endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")

    add_custom_command(OUTPUT ${expat_files}
                        COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_SOURCE_DIR}/libs/expat-${EXPAT_VER}.tar.gz
                        COMMAND ${CMAKE_COMMAND} -E chdir ${EXPAT_TAR_DIR} ${expat_config_command}
                        COMMAND ${CMAKE_COMMAND} -E chdir ${expat_build_dir} ${expat_make_command}
                        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                        VERBATIM
                        )

    add_custom_target(custom_expat DEPENDS ${expat_files})
endif (EXISTS ${CMAKE_SOURCE_DIR}/libs/bin/${CMAKE_SYSTEM_NAME}/expat)

MESSAGE(STATUS "Custom expat includes in -- ${CUSTOM_EXPAT_INCLUDE}")
MESSAGE(STATUS "Custom expat library in  -- ${CUSTOM_EXPAT_LIB}")
MESSAGE(STATUS "Searching for expat binaries - done")

# libedit "external" library
# we use internal cached variables to determine directories for -I and -L
set(LIBEDIT_VER "20090610-3.0")
message(STATUS "Will build libedit from scratch when building the project, if needed...")

set(LIBEDIT_TAR_DIR ${CMAKE_BINARY_DIR}/libs/libedit-${LIBEDIT_VER})
set(libedit_files ${LIBEDIT_TAR_DIR}/include/editline/readline.h ${LIBEDIT_TAR_DIR}/lib/libedit${CMAKE_STATIC_LIBRARY_SUFFIX})

set(CUSTOM_LIBEDIT_INCLUDE ${LIBEDIT_TAR_DIR}/include/editline CACHE INTERNAL "Internal libedit include path")
set(CUSTOM_LIBEDIT_LIB ${LIBEDIT_TAR_DIR}/lib CACHE INTERNAL "Internal libedit library path")

add_custom_command(OUTPUT ${libedit_files}
                    COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_SOURCE_DIR}/libs/libedit-${LIBEDIT_VER}.tar.gz
                    COMMAND ${CMAKE_COMMAND} -E chdir ${LIBEDIT_TAR_DIR} sh configure --enable-shared=no --prefix=${LIBEDIT_TAR_DIR}
                    COMMAND ${CMAKE_COMMAND} -E chdir ${LIBEDIT_TAR_DIR} make install
                    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                    VERBATIM
                    )

add_custom_target(custom_libedit DEPENDS ${libedit_files})

MESSAGE(STATUS "Custom libedit includes in -- ${CUSTOM_LIBEDIT_INCLUDE}")
MESSAGE(STATUS "Custom libedit library in  -- ${CUSTOM_LIBEDIT_LIB}")

