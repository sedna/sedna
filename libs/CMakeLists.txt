# pcre library
add_subdirectory(pcre)

#ancillary library
if(UNIX)
add_subdirectory(libancillary-0.9.1)
endif(UNIX)

# expat "external" library
# we use internal cached variables to determine directories for -I and -L: 1) look for cache 2) then try to describe builduing expat from scratch
set(EXPAT_VER "2.0.1")

message(STATUS "Configuring custom expat, version: ${EXPAT_VER}, platform: ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

# directory to untar expat
set(EXPAT_TAR_DIR ${CMAKE_BINARY_DIR}/libs/expat-${EXPAT_VER})

# expat files we depend on
set(expat_files_lib ${EXPAT_TAR_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}expat${CMAKE_STATIC_LIBRARY_SUFFIX})
set(expat_files_src ${EXPAT_TAR_DIR}/lib/expat.h ${EXPAT_TAR_DIR}/lib/expat_external.h
                    ${EXPAT_TAR_DIR}/lib/xmlparse.c ${EXPAT_TAR_DIR}/lib/xmltok.c ${EXPAT_TAR_DIR}/lib/xmlrole.c)

# command to obtain source files
add_custom_command(OUTPUT ${expat_files_src}
                   COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_SOURCE_DIR}/libs/expat-${EXPAT_VER}.tar.gz
                   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                   VERBATIM
                   )

# full library path (exported to parent via INTERNAL)
set(CUSTOM_EXPAT_DIR ${EXPAT_TAR_DIR}/lib CACHE INTERNAL "Internal expat full path")

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    add_library(expat STATIC ${expat_files_src})
    set_property(TARGET expat PROPERTY COMPILE_DEFINITIONS _MBCS _LIB COMPILED_FROM_DSP XML_LARGE_SIZE)
    add_custom_target(custom_expat DEPENDS expat)
else (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    # on unix/linux we just use configure+make
    add_custom_command(OUTPUT ${expat_files_lib}
                       COMMAND /bin/sh configure --enable-shared=no --prefix=${EXPAT_TAR_DIR} CPPFLAGS=-DXML_LARGE_SIZE
                       COMMAND make install
                       DEPENDS ${expat_files_src}
                       WORKING_DIRECTORY ${EXPAT_TAR_DIR}
                       VERBATIM
                      )

    # this and the above command to avoid rebuilding expat
    add_custom_target(custom_expat DEPENDS ${expat_files_lib})
endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")

MESSAGE(STATUS "Custom expat in -- ${EXPAT_TAR_DIR}/lib")
MESSAGE(STATUS "Configuring custom expat - done")

# libedit "external" library
# we use internal cached variables to determine directories for -I and -L
set(LIBEDIT_VER "20100424-3.0")
message(STATUS "Will build libedit from scratch when building the project, if needed...")

set(LIBEDIT_TAR_DIR ${CMAKE_BINARY_DIR}/libs/libedit-${LIBEDIT_VER})
set(libedit_files ${LIBEDIT_TAR_DIR}/include/editline/readline.h ${LIBEDIT_TAR_DIR}/lib/libedit${CMAKE_STATIC_LIBRARY_SUFFIX})

set(CUSTOM_LIBEDIT_INCLUDE ${LIBEDIT_TAR_DIR}/include/editline CACHE INTERNAL "Internal libedit include path")
set(CUSTOM_LIBEDIT_LIB ${LIBEDIT_TAR_DIR}/lib CACHE INTERNAL "Internal libedit library path")

if (UNIX)
    find_program(PATCH_COMMAND patch)
    if (NOT PATCH_COMMAND)
        message(STATUS "Cannot find patch -- it'll not be possible to build libedit")
    endif (NOT PATCH_COMMAND)
endif (UNIX)


add_custom_command(OUTPUT ${libedit_files}
                   COMMAND ${CMAKE_COMMAND} -E tar xzf ${CMAKE_SOURCE_DIR}/libs/libedit-${LIBEDIT_VER}.tar.gz
                   COMMAND ${PATCH_COMMAND} -p0 -d ${LIBEDIT_TAR_DIR} < ${CMAKE_CURRENT_SOURCE_DIR}/01-libedit-dups.patch
                   COMMAND ${PATCH_COMMAND} -p0 -d ${LIBEDIT_TAR_DIR} < ${CMAKE_CURRENT_SOURCE_DIR}/02-libedit-bsd.patch
                   COMMAND ${CMAKE_COMMAND} -E chdir ${LIBEDIT_TAR_DIR} sh configure --enable-shared=no --prefix=${LIBEDIT_TAR_DIR}
                   COMMAND ${CMAKE_COMMAND} -E chdir ${LIBEDIT_TAR_DIR} make install
                   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                   VERBATIM
                  )

add_custom_target(custom_libedit DEPENDS ${libedit_files})

MESSAGE(STATUS "Custom libedit includes in -- ${CUSTOM_LIBEDIT_INCLUDE}")
MESSAGE(STATUS "Custom libedit library in  -- ${CUSTOM_LIBEDIT_LIB}")
