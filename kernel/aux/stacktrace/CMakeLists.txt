set(st_SRC stacktrfmt.c)

if ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    list(APPEND st_SRC os_nt/stacktrace.c)
else ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    list(APPEND st_SRC os_other/stacktrace.c)
endif ("${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
