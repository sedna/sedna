# try to find Java
find_package(Java)

# try to find javadoc
if (JAVA_COMPILE)
    get_filename_component(JAVA_PATH ${JAVA_COMPILE} PATH)
    find_program(JAVA_DOC NAMES javadoc PATHS ${JAVA_PATH})
endif (JAVA_COMPILE)

# if we don't have it then disable building Java driver
if (NOT JAVA_COMPILE OR NOT JAVA_ARCHIVE OR NOT JAVA_DOC)
    message(STATUS "Cannot build Sedna Java driver -- cannot find javac, jar or javadoc")
    set(JAVA_DRIVER OFF)
else (NOT JAVA_COMPILE OR NOT JAVA_ARCHIVE OR NOT JAVA_DOC)
    message(STATUS "Java compiler found -- ${JAVA_COMPILE}")
    message(STATUS "Java Archive tool found -- ${JAVA_ARCHIVE}")
    message(STATUS "Java Doc generator found -- ${JAVA_DOC}")
endif (NOT JAVA_COMPILE OR NOT JAVA_ARCHIVE OR NOT JAVA_DOC)

# create rules to build Java driver
if (JAVA_DRIVER)
    set(java_driver_SRC src/ru/ispras/sedna/driver/DriverException.java
                        src/ru/ispras/sedna/driver/ResultType.java
                        src/ru/ispras/sedna/driver/NetOps.java
                        src/ru/ispras/sedna/driver/SednaSerializedResult.java
                        src/ru/ispras/sedna/driver/SednaSerializedResultImpl.java
                        src/ru/ispras/sedna/driver/SednaStatement.java
                        src/ru/ispras/sedna/driver/SednaStatementImpl.java
                        src/ru/ispras/sedna/driver/SednaConnection.java
                        src/ru/ispras/sedna/driver/SednaConnectionImpl.java
                        src/ru/ispras/sedna/driver/DatabaseManager.java
                        src/ru/ispras/sedna/driver/ResultInterceptor.java
                        src/ru/ispras/sedna/driver/DefaultResultInterceptor.java
                        ${CMAKE_CURRENT_BINARY_DIR}/src/ru/ispras/sedna/driver/Debug.java
                        ${CMAKE_CURRENT_BINARY_DIR}/src/ru/ispras/sedna/driver/ErrorCodes.java)

    set_source_files_properties(${CMAKE_CURRENT_BINARY_DIR}/src/ru/ispras/sedna/driver/ErrorCodes.java PROPERTIES GENERATED 1)

    # set debug option as a constant for java class
    if (EL_DEBUG)
        set(JAVA_DRIVER_DEBUG true)
        set(JAVAC_OPTIONS -g) # add debug option to java compiler
    else (EL_DEBUG)
        set(JAVA_DRIVER_DEBUG false)
    endif (EL_DEBUG)
    configure_file(src/ru/ispras/sedna/driver/Debug.java.in ${CMAKE_CURRENT_BINARY_DIR}/src/ru/ispras/sedna/driver/Debug.java @ONLY)

    # custom target for the driver
    add_custom_command(
        OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/lib/sednadriver.jar
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/out
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/lib
        COMMAND ${JAVA_COMPILE} ${JAVAC_OPTIONS} -d ${CMAKE_CURRENT_BINARY_DIR}/out ${java_driver_SRC}
        COMMAND ${JAVA_DOC} -d ${CMAKE_CURRENT_BINARY_DIR}/javadoc ${java_driver_SRC}
        COMMAND ${JAVA_ARCHIVE} cf ${CMAKE_CURRENT_BINARY_DIR}/lib/sednadriver.jar -C ${CMAKE_CURRENT_BINARY_DIR}/out ru
        DEPENDS ${java_driver_SRC}
        COMMENT "Generating Sedna Java driver and Java Docs"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

    add_custom_target(sedna_java_driver ALL
        DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/lib/sednadriver.jar
    )

    add_dependencies(sedna_java_driver generate_error_codes)
    set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "out;lib;javadoc")

    # install rules - just copy directories
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/javadoc
                      ${CMAKE_CURRENT_BINARY_DIR}/lib
            DESTINATION driver/java)

    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src src
            DESTINATION driver/java
            FILES_MATCHING PATTERN "*.java")

endif (JAVA_DRIVER)
