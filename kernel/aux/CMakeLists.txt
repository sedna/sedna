# full common library for sedna
set(aux_SRC )

ADD_SUBFOLDER_SRC(aux_SRC argtable)
ADD_SUBFOLDER_SRC(aux_SRC options)
ADD_SUBFOLDER_SRC(aux_SRC stacktrace)

SET_SOURCES(aux_SRC 1)

add_library(aux STATIC ${aux_SRC})

add_dependencies(aux u) 
# find/build expat before common
add_dependencies(aux custom_expat) 
# build error codes before common (devenv parallel build freaks out without this)
add_dependencies(aux generate_error_codes) 
