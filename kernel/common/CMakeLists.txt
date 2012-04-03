# full common library for sedna
set(common_SRC argtable.c base.cpp bit_set.cpp gmm.cpp ipc_ops.cpp pping.cpp rcv_test.cpp sedna.c sp.c SSMMsg.cpp tr_debug.cpp ugc.cpp utils.cpp version.c xptr.cpp)

ADD_SUBFOLDER_SRC(common_SRC errdbg)
ADD_SUBFOLDER_SRC(common_SRC llcommon)
ADD_SUBFOLDER_SRC(common_SRC mmgr)
ADD_SUBFOLDER_SRC(common_SRC st)
ADD_SUBFOLDER_SRC(common_SRC u)

SET_SOURCES(common_SRC 1)

add_library(common STATIC ${common_SRC})
add_dependencies(common custom_expat) # find/build expat before common
if(UNIX)
add_dependencies(common ancillary) # find/build ancillary lib before common
endif(UNIX)
add_dependencies(common generate_error_codes) # build error codes before common (devenv parallel build freaks out without this)
