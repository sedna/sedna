# Sedna protocol version
set(SEDNA_PROTOCOL_VERSION 3.0.0)
message(STATUS "Sedna protocol version is -- ${SEDNA_PROTOCOL_VERSION}")

# c driver
add_subdirectory(c)

# java driver
if (JAVA_DRIVER)
    add_subdirectory(java)
endif (JAVA_DRIVER)

# install for Scheme driver
install(DIRECTORY scheme/ DESTINATION driver/scheme)
