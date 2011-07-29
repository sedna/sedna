CHECK_FUNCTION_EXISTS(strnlen __STRNLEN_DEFINED)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/u/uni.h.in ${CMAKE_CURRENT_BINARY_DIR}/u/uni.h)

set(u_SRC u.c uevent.c ugnames.cpp uhdd.c ushm.c ummap.c umutex.c uprocess.c usecurity.c usem.c usocket.c usystem.c uthread.c utime.c uutils.c)
