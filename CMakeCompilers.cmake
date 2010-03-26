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
    if (CMAKE_COMPILER_2005)
        add_definitions(-D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE)
    endif (CMAKE_COMPILER_2005)

    # enable static run-time linking
    if (MS_LINK_RUNTIME_STATIC)
        switch_compiler_flag("/MD" "/MT")
    endif (MS_LINK_RUNTIME_STATIC)

    # set some /NODEFAULT libs
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:LIBCMT.LIB")
endif(MSVC)

# GNU compiler
if (CMAKE_COMPILER_IS_GNUCXX)
    # first, add "-ggdb -g3" flag to generate more debug info
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g3 -ggdb")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g3 -ggdb")

    # add some -f declarations
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing -fno-omit-frame-pointer")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing -fexceptions -fno-omit-frame-pointer")

    # add gcov
    if (SE_ENABLE_GCOV)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fprofile-arcs -ftest-coverage")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fprofile-arcs -ftest-coverage")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fprofile-arcs -ftest-coverage")
    endif (SE_ENABLE_GCOV)

    # set warning level
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-deprecated")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wno-unused -Wno-uninitialized")

    # force 32-bit buid on 64-bit architectures
    # temporal solution here: FORCE_64_BIT_COMPILE will force 64-bit compilation (needed for 64-bit driver build)
    if (CMAKE_SIZEOF_VOID_P MATCHES 8 AND GCC_USE_M32)
        message(STATUS "Forcing 32-bit compilation on 64-bit platform...")
        find_file(GCC_32_STUB "gnu/stubs-32.h")
        if (NOT GCC_32_STUB)
            message(STATUS "Stub file for compiling 32-bit code on 64-bit platform not found. Try to install gcc-multilib")
        endif (NOT GCC_32_STUB)

        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")
    endif (CMAKE_SIZEOF_VOID_P MATCHES 8 AND GCC_USE_M32)
endif(CMAKE_COMPILER_IS_GNUCXX)
