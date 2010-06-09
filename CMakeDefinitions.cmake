# stdint definitions (see c++ standard)
add_definitions(-D__STDC_CONSTANT_MACROS)
add_definitions(-D__STDC_FORMAT_MACROS)
add_definitions(-D__STDC_LIMIT_MACROS)

# enable 64-bit integer support for line and column
# numbers and the over-all byte index for Expat
add_definitions(-DXML_LARGE_SIZE)

# lfs extensions to glibc
if (UNIX)
    add_definitions(-D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64)
endif (UNIX)

if ("${CMAKE_SYSTEM_NAME}" MATCHES "SunOS")
    add_definitions(-D_POSIX_PTHREAD_SEMANTICS -D_REENTRANT)
endif ("${CMAKE_SYSTEM_NAME}" MATCHES "SunOS")

if ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    add_definitions(-D_CONSOLE)   # needed? CMake probably inserts this anyway
    add_definitions(-DXML_STATIC) # needed for expat
    add_definitions(-DNOMINMAX)   # don't let windows.h to define min-max macroses
endif ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
