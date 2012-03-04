# Some compiler options

# macros that switch flag 'flag_src' on flag 'flag_dest' in compiler flags for ALL configurations
macro (switch_compiler_flag flag_src flag_dest)
    foreach(flag
            CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
            CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
        if(${flag} MATCHES "${flag_src}")
            string(REGEX REPLACE "${flag_src}" "${flag_dest}" ${flag} "${${flag}}")
        endif(${flag} MATCHES "${flag_src}")
    endforeach(flag)

    foreach(flag
            CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
            CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)
        if(${flag} MATCHES "${flag_src}")
            string(REGEX REPLACE "${flag_src}" "${flag_dest}" ${flag} "${${flag}}")
        endif(${flag} MATCHES "${flag_src}")
    endforeach(flag)
endmacro (switch_compiler_flag flag_src flag_dest)

# Microsoft compiler
if (MSVC)
    # remove annoying warnings
    add_definitions(-D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS)
    # enable static run-time linking
    if (MS_LINK_RUNTIME_STATIC)
        switch_compiler_flag("/MD" "/MT")
    endif (MS_LINK_RUNTIME_STATIC)

    # set some /NODEFAULT libs
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:LIBCMT.LIB")
endif(MSVC)

# GNU compiler
if (CMAKE_COMPILER_IS_GNUCXX)
    exec_program(${CMAKE_CXX_COMPILER} ARGS --version OUTPUT_VARIABLE _GCC_COMPILER_VERSION)
    string(REGEX REPLACE ".* ([0-9])\\.([0-9])\\.[0-9] .*" "\\1\\2"
                   _GCC_COMPILER_VERSION ${_GCC_COMPILER_VERSION})
    message("-- GCC version -- ${_GCC_COMPILER_VERSION}")

    # first, add "-ggdb -g3" flag to generate more debug info
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g3 -ggdb")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g3 -ggdb")

    # turn off tree-vectorize O3 optimization; it breaks Sedna
    # gcc versions 3.x.x lack this option thus we need this condition
    # gcc version is 4.6.1 at this moment
    if (_GCC_COMPILER_VERSION STRGREATER "40")
      set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -fno-tree-vectorize")
      set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -fno-tree-vectorize")
    endif (_GCC_COMPILER_VERSION STRGREATER "40")

    # add some -f declarations
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing -fno-omit-frame-pointer")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing -fexceptions -fno-omit-frame-pointer")

    # add gcov
    if (SE_ENABLE_GCOV)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-arcs -ftest-coverage --coverage")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fprofile-arcs -ftest-coverage --coverage")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fprofile-arcs -ftest-coverage --coverage")
    endif (SE_ENABLE_GCOV)

    # set warning level
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unused -Wno-deprecated")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wno-unused -Wno-uninitialized")

    if (_GCC_COMPILER_VERSION STRGREATER "40")
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wextra -Wno-missing-field-initializers")
    endif (_GCC_COMPILER_VERSION STRGREATER "40")

endif(CMAKE_COMPILER_IS_GNUCXX)
