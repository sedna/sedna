# additional sub-libraries
# set(lib_SUBDIRS xqp auth cat executor crmutils idx locks log mo nid pstr rcv strings structures updates vmm bstrie btree)
set(lib_SUBDIRS vmm executor)

# add ftsearch
# if (ENABLE_FTSEARCH)
#     list(APPEND lib_SUBDIRS ft)
# endif (ENABLE_FTSEARCH)
# 
# # dtsearch-specific headers and sources
# if (ENABLE_DTSEARCH)
#     file(GLOB dt_headers ${DTSEARCH_INCLUDES}/dtsearch/include/*.h)
#     set(dt_sources ${DTSEARCH_COMMON}/dtsviewr.cpp ${DTSEARCH_COMMON}/dtsearch.cpp)
# 
#     source_group("dts" FILES ${dt_sources} ${dt_headers})
# endif (ENABLE_DTSEARCH)

# add triggers
# if (ENABLE_TRIGGERS)
#     list(APPEND lib_SUBDIRS triggers)
# endif (ENABLE_TRIGGERS)
# 
# # additional include directories for xqp relative paths
# include_directories(${CMAKE_CURRENT_BINARY_DIR}/xqp) # because we use relative paths to generated files there
# include_directories(${CMAKE_CURRENT_SOURCE_DIR}/xqp) # because we use relative paths there

# dtsearch paths and definitions
# if (ENABLE_DTSEARCH)
#     include_directories(${DTSEARCH_INCLUDES})
#     include_directories(${DTSEARCH_INCLUDES}/dtsearch/include)
# 
#     if (UNIX)
#         add_definitions(-D__UNIX__ -DUnicodeText)
#     endif (UNIX)
# endif (ENABLE_DTSEARCH)

# recursively set se_trn sources
#set(trn_SRC tr.cpp socket_client.cpp cl_client.cpp tr_functions.cpp tr_common_funcs.cpp tr_globals.cpp)
set(trn_SRC tr.cpp tr_globals.cpp)
foreach (SUB ${lib_SUBDIRS})
    ADD_SUBFOLDER_SRC(trn_SRC ${SUB})
endforeach (SUB ${lib_SUBDIRS})
SET_SOURCES(trn_SRC 1)

# check for FlexLexer.h
# if (NOT USING_PREGEN_LEXER)
#     find_path(FLEXLEXER_PATH "FlexLexer.h")
# endif (NOT USING_PREGEN_LEXER)

# if we haven't found FlexLexer.h or we are using our own lexer files, use our
# own FlexLexer.h as well
# if (NOT FLEXLEXER_PATH)
#     message(STATUS "FlexLexer.h file is not found or isn't needed -- using the
#         one from ${CMAKE_SOURCE_DIR}/libs/compat/flex")
#     include_directories(${CMAKE_SOURCE_DIR}/libs/compat/flex)
# else (NOT FLEXLEXER_PATH)
#     message(STATUS "Found FlexLexer.h in -- ${FLEXLEXER_PATH}")
# endif (NOT FLEXLEXER_PATH)

# libraries to link
set(common_libs common expat pcre ${sysaux_LIB})

# dtsearch libs
# if (ENABLE_DTSEARCH)
#     list(APPEND common_libs ${DTSEARCH_LIB})
#     list(APPEND trn_SRC ${dt_sources} ${dt_headers})
# 
#     # add zlib on Unix
#     if (UNIX)
#         list(APPEND common_libs ${Z_LIB})
#     endif (UNIX)
# endif (ENABLE_DTSEARCH)

# se_trn
add_executable(se_trn ${trn_SRC})
target_link_libraries(se_trn ${common_libs})

add_dependencies(se_trn generate_error_codes) # generate error files before se_trn
add_dependencies(se_trn custom_expat)         # find/build expat before se_trn

# msvc-specific options: 
#    1) it seems that 10mb stack is not enough for debug modes, make it 25mb
#       note that there are software checks on executor, but in debug modes
#       frame-size is larger than expected (especially on x64)
#    2) we want to turn off ASLR since it might mess up our layer architecture
#    3) we add /LARGEADDRESSAWARE (Handle Large Addresses). it seems to make
#    life easier on x64
if (MSVC)
    set(trn_msvc_linker_flags "/LARGEADDRESSAWARE")
    if (NOT MSVC_VERSION LESS 1500)
        set(trn_msvc_linker_flags "${trn_msvc_linker_flags} /DYNAMICBASE:NO")     # turn off ASLR
    endif (NOT MSVC_VERSION LESS 1500)
    set_target_properties(se_trn PROPERTIES LINK_FLAGS "${trn_msvc_linker_flags}" LINK_FLAGS_DEBUG "/STACK:25000000")
endif (MSVC)

# put in build/bin
FinalizeBinaries(se_trn)

# install trn in bin
install(TARGETS se_trn RUNTIME DESTINATION bin)

