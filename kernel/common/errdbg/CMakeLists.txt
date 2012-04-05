set(errdbg_SRC assert.c d_printf.c event_log.c errors.c exceptions.cpp error_codes.c error_codes.h)

set(errdbg_GENERATED ${CMAKE_CURRENT_BINARY_DIR}/errdbg/error_codes.c ${CMAKE_CURRENT_BINARY_DIR}/errdbg/error_codes.h
                     ${CMAKE_CURRENT_BINARY_DIR}/errdbg/error_codes_scm.scm ${CMAKE_CURRENT_BINARY_DIR}/errdbg/ErrorCodes.java
                     ${CMAKE_BINARY_DIR}/driver/java/src/ru/ispras/sedna/driver/ErrorCodes.java)
source_group(errdbg FILES ${errdbg_GENERATED})

# create error codes generator and place it to errdbg directory
add_executable(gen_error_codes EXCLUDE_FROM_ALL errdbg/gen_error_codes.c)
source_group("" FILES errdbg/gen_error_codes.c)
set_target_properties(gen_error_codes PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/errdbg)

# to generate error code files via our generator
file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/errdbg/error.codes error_codes_path)
add_custom_command(
    OUTPUT ${errdbg_GENERATED}
    COMMAND gen_error_codes ${error_codes_path}
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/errdbg/ErrorCodes.java ${CMAKE_BINARY_DIR}/driver/java/src/ru/ispras/sedna/driver/ErrorCodes.java
    DEPENDS gen_error_codes errdbg/error.codes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/errdbg
)

# target that generates error codes file for various languages; useful to specify dependencies
add_custom_target(generate_error_codes DEPENDS ${errdbg_GENERATED})
