if (UNIX)
SET(ancillary_SRC fd_recv.c fd_send.c)

SET_SOURCES(ancillary_SRC 1)
ADD_LIBRARY(ancillary STATIC ${ancillary_SRC})
endif (UNIX)