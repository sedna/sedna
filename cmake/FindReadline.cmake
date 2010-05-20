# - Find realine
#
#  READLINE_INCLUDE_DIR - where to find readline headers
#  READLINE_LIBRARIES   - List of libraries when using readline
#  READLINE_FOUND       - True if readline is found


IF (READLINE_INCLUDE_DIR)
  # Already in cache
  SET(Readline_FIND_QUIETLY TRUE)
ENDIF (READLINE_INCLUDE_DIR)

FIND_PATH(READLINE_INCLUDE_DIR readline/readline.h readline.h)
FIND_LIBRARY(READLINE_LIBRARIES readline)

IF(READLINE_INCLUDE_DIR AND READLINE_LIBRARIES)
   SET(READLINE_FOUND TRUE)
ELSE(READLINE_INCLUDE_DIR AND READLINE_LIBRARIES)
   SET(READLINE_FOUND FALSE)
ENDIF (READLINE_INCLUDE_DIR AND READLINE_LIBRARIES)

IF (READLINE_FOUND)
   IF (NOT Readline_FIND_QUIETLY)
      MESSAGE(STATUS "Found Readline: ${READLINE_LIBRARIES}")
      MESSAGE(STATUS "Found Readline headers in: ${READLINE_INCLUDE_DIR}")
   ENDIF (NOT Readline_FIND_QUIETLY)
ELSE (READLINE_FOUND)
   IF (Readline_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR "Could NOT find Readline")
   ENDIF (Readline_FIND_REQUIRED)
ENDIF (READLINE_FOUND)

MARK_AS_ADVANCED(READLINE_LIBRARIES READLINE_INCLUDE_DIR)
