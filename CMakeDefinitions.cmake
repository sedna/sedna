# lfs extensions to glibc
if (UNIX)
    add_definitions(-D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64)
endif (UNIX)

if ("${CMAKE_SYSTEM_NAME}" MATCHES "SunOS")
    add_definitions(-D_POSIX_PTHREAD_SEMANTICS)
endif ("${CMAKE_SYSTEM_NAME}" MATCHES "SunOS")

if ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    add_definitions(-D_CONSOLE)   # needed? CMake probably inserts this anyway
    add_definitions(-DXML_STATIC) # needed for expat
    add_definitions(-DNOMINMAX) # don't let windows.h to define min-max macroses
endif ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
