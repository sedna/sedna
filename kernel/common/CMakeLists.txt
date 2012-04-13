# full common library for sedna
set(common_SRC )

ADD_SUBFOLDER_SRC(common_SRC errdbg)
ADD_SUBFOLDER_SRC(common_SRC globalobjects)
ADD_SUBFOLDER_SRC(common_SRC llcommon)
ADD_SUBFOLDER_SRC(common_SRC procutils)
ADD_SUBFOLDER_SRC(common_SRC protocol)
ADD_SUBFOLDER_SRC(common_SRC socketutils)
ADD_SUBFOLDER_SRC(common_SRC ssmmsg)
ADD_SUBFOLDER_SRC(common_SRC structures)

SET_SOURCES(common_SRC 1)

add_library(common STATIC ${common_SRC})
add_dependencies(common custom_expat) # find/build expat before common
add_dependencies(common u) # build u
if(UNIX)
add_dependencies(common ancillary) # find/build ancillary lib before common
endif(UNIX)
add_dependencies(common generate_error_codes) # build error codes before common (devenv parallel build freaks out without this)

target_link_libraries(common u)
target_link_libraries(common ancillary)
target_link_libraries(common aux)
