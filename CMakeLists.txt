# The name of our project is "SEDNA".  CMakeLists files in this project can
# refer to the root source directory of the project as ${SEDNA_SOURCE_DIR} and
# to the root binary directory of the project as ${SEDNA_BINARY_DIR}.
project (SEDNA)

IF(CYGWIN)
ADD_DEFINITIONS(-D__cygwin__)
ELSEIF(WIN32)
ADD_DEFINITIONS(-DWIN32 -D_WIN32)
ENDIF(CYGWIN)


ADD_DEFINITIONS(-DSEDNA_VERSION=\\"1\\" -DSEDNA_BUILD=\\"99\\")

# Recurse into ... subdirectories.  This does not actually
# cause another cmake executable to run.  The same process will walk through
# the project's entire directory structure.
add_subdirectory (kernel)
add_subdirectory (driver)
add_subdirectory (term)
add_subdirectory (export)
