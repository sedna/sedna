# full common library for sedna
set(auxiliary_SRC )

ADD_SUBFOLDER_SRC(auxiliary_SRC argtable2)
ADD_SUBFOLDER_SRC(auxiliary_SRC options)
ADD_SUBFOLDER_SRC(auxiliary_SRC stacktrace)

SET_SOURCES(auxiliary_SRC 1)

add_library(auxiliary STATIC ${auxiliary_SRC})

add_dependencies(auxiliary u) 
# find/build expat before common
add_dependencies(auxiliary custom_expat) 

target_link_libraries(auxiliary expat)
