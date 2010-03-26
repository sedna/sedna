set(fo_SRC boolean_operations.cpp casting_operations.cpp
       comparison_operations.cpp dateTime_operations.cpp
       op_map.cpp other_fo.cpp string_operations.cpp
       numeric_operations.cpp numeric_operations.h)

set(fo_GENERATED ${CMAKE_CURRENT_BINARY_DIR}/executor/fo/numeric_operations.cpp ${CMAKE_CURRENT_BINARY_DIR}/executor/fo/numeric_operations.h)
source_group("executor\\fo" FILES ${fo_GENERATED})

# create numeric_operations generator and place it to fo build directory
add_executable(gen_num_ops EXCLUDE_FROM_ALL executor/fo/gen_num_ops.c)
source_group("" FILES executor/fo/gen_num_ops.c)
set_target_properties(gen_num_ops PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/executor/fo)

# to generate numeric operations files via our generator
add_custom_command(
    OUTPUT ${fo_GENERATED}
    COMMAND gen_num_ops
    DEPENDS gen_num_ops
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/executor/fo
)

