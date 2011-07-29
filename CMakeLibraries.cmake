include(CheckLibraryExists)
include(CheckFunctionExists)

message(STATUS "Searching for system libraries")
set(sysaux_LIB)

if (NOT "${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    # first, determine shm_open library
    check_function_exists("shm_open" CMAKE_HAVE_SHMOPEN)
    if (NOT CMAKE_HAVE_SHMOPEN)
        check_library_exists("rt" "shm_open" "" CMAKE_LIB_RT_HAS_SHMOPEN)
        if (CMAKE_LIB_RT_HAS_SHMOPEN)
            list(APPEND sysaux_LIB rt)
        endif (CMAKE_LIB_RT_HAS_SHMOPEN)
    endif (NOT CMAKE_HAVE_SHMOPEN)

    # then, determin dl-funcs library
    check_function_exists("dlopen" CMAKE_HAVE_DLOPEN)
    if (NOT CMAKE_HAVE_DLOPEN)
        check_library_exists("dl" "dlopen" "" CMAKE_LIB_DL_HAS_DLOPEN)
        if (CMAKE_LIB_DL_HAS_DLOPEN)
            list(APPEND sysaux_LIB dl)
        endif (CMAKE_LIB_DL_HAS_DLOPEN)
    endif (NOT CMAKE_HAVE_DLOPEN)

    # link to pthread library if needed (except for FreeBSD)
    check_function_exists("pthread_create" CMAKE_HAVE_PTHREAD_CREATE)
    if (NOT CMAKE_HAVE_PTHREAD_CREATE)
        check_library_exists("thr" "pthread_create" "" CMAKE_LIB_THR_HAS_PTHREAD_CREATE)
        if (CMAKE_LIB_THR_HAS_PTHREAD_CREATE)
            list(APPEND sysaux_LIB thr)
        else (CMAKE_LIB_THR_HAS_PTHREAD_CREATE)
            check_library_exists("pthread" "pthread_create" "" CMAKE_LIB_PTHREAD_HAS_PTHREAD_CREATE)
            if (CMAKE_LIB_PTHREAD_HAS_PTHREAD_CREATE)
                list(APPEND sysaux_LIB pthread)
            endif (CMAKE_LIB_PTHREAD_HAS_PTHREAD_CREATE)
        endif (CMAKE_LIB_THR_HAS_PTHREAD_CREATE)
    endif (NOT CMAKE_HAVE_PTHREAD_CREATE)

    # check if we need socket library
    check_function_exists("connect" CMAKE_HAVE_CONNECT)
    if (NOT CMAKE_HAVE_CONNECT)
        check_library_exists("socket" "connect" "" CMAKE_LIB_SOCKET_HAS_CONNECT)
        if (CMAKE_LIB_SOCKET_HAS_CONNECT)
            list(APPEND sysaux_LIB socket)
        endif (CMAKE_LIB_SOCKET_HAS_CONNECT)
    endif (NOT CMAKE_HAVE_CONNECT)

    # check for nsl library
    check_function_exists("getaddrinfo" CMAKE_HAVE_GETADDRINFO)
    if (NOT CMAKE_HAVE_GETADDRINFO)
        check_library_exists("nsl" "getaddrinfo" "" CMAKE_LIB_NSL_HAS_GETADDRINFO)
        if (CMAKE_LIB_NSL_HAS_GETADDRINFO)
            list(APPEND sysaux_LIB nsl)
        else (CMAKE_LIB_NSL_HAS_GETADDRINFO)
            check_library_exists("bsd" "getaddrinfo" "" CMAKE_LIB_BSD_HAS_GETADDRINFO)
            if (CMAKE_LIB_BSD_HAS_GETADDRINFO)
                list(APPEND sysaux_LIB bsd)
            endif (CMAKE_LIB_BSD_HAS_GETADDRINFO)
        endif (CMAKE_LIB_NSL_HAS_GETADDRINFO)
    endif (NOT CMAKE_HAVE_GETADDRINFO)

    message(STATUS "Needed system libraries -- ${sysaux_LIB}")
else (NOT "${CMAKE_SYSTEM_NAME}" MATCHES "Windows")
    set(sysaux_LIB WS2_32)

    # add library for stack tracing
    if (EL_DEBUG)
        list(APPEND sysaux_LIB Dbghelp)
    endif (EL_DEBUG)
endif (NOT "${CMAKE_SYSTEM_NAME}" MATCHES "Windows")

message(STATUS "Searching for system libraries -- done")
