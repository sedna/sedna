# This macros allows collecting sources for "treelike"-libraries (e.g. lib->a->b, when we want all sources from lib and its subtree to appear in one library in root of lib)
# First, you should define variable with name <current_subfolder>_SRC and initialize it with this subfolfer's sources (e.g. SET(a_SRC a.cpp) then use can use this macros
# to collect sources from subdirectories (e.g ADD_SUBFOLDER_SRC(a_SRC b))
macro (ADD_SUBFOLDER_SRC DEST SUBFOLDER)
    list(APPEND REL_PATH_STACK ${SUBFOLDER})
    GET_REL_PATH(${SUBFOLDER})

    include (${REL_PATH}/CMakeLists.txt) # REL_PATH may be changed here!

    GET_REL_PATH(${SUBFOLDER})

    # name for subfolder sources (convention: <subfolder_name>_SRC)
    set(SUBFOLDER_SRC_NAME ${SUBFOLDER}_SRC)

    # to make CMake find source files from library root directory we must prepend it recursively with path components and only then add it recursively to DEST
    foreach (SRCS ${${SUBFOLDER_SRC_NAME}})
        list(APPEND ${DEST} ${SUBFOLDER}/${SRCS})
    endforeach (SRCS)

    list(REMOVE_AT REL_PATH_STACK -1)
endmacro (ADD_SUBFOLDER_SRC DEST SUBFOLDER)

# get relative path to find CMakeLists.txt for SUBFOLDER (see macros above)
macro (GET_REL_PATH SUBFOLDER)
    set(REL_PATH)
    foreach(arg ${REL_PATH_STACK})
        if (NOT REL_PATH)
            set(REL_PATH "${arg}")
        else (NOT REL_PATH)
            set(REL_PATH "${REL_PATH} ${arg}")
        endif (NOT REL_PATH)
    endforeach(arg)
    string(REPLACE " " "/" REL_PATH ${REL_PATH})
endmacro (GET_REL_PATH SUBFOLDER)

# This macros adds header files to "dest_src" source files and sets Visual Studio folders in more or less sane manner
macro (SET_SOURCES dest_src get_recurse_headers)
    if (${get_recurse_headers})
        file(GLOB_RECURSE h_srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h *.hpp *.hh)
    else (${get_recurse_headers})
        file(GLOB h_srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.h *.hpp *.hh)
    endif (${get_recurse_headers})
    list(APPEND ${dest_src} ${h_srcs})
    
    foreach (src ${${dest_src}})
        get_filename_component(src_grp ${src} PATH)
        
        if (src_grp)
            string(REPLACE "/" "\\" src_grp ${src_grp})
            source_group(${src_grp} FILES ${src})
        else (src_grp)
            source_group("" FILES ${src})
        endif (src_grp)
    endforeach (src)
endmacro (SET_SOURCES dest_src)
