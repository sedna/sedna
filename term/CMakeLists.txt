# if user wants it, try to link with readline
if (NOT ${ENHANCE_TERM} STREQUAL "None")
    if (${ENHANCE_TERM} STREQUAL "Readline")
        find_package(Readline)
        if (NOT READLINE_FOUND)
            message(STATUS "Cannot find readline library -- falling back to libedit")
            set(ENHANCE_TERM "Libedit")
        else (NOT READLINE_FOUND)
            set(TERM_LIB ${READLINE_LIBRARIES})
            include_directories(${READLINE_INCLUDE_DIR})
            include_directories(${READLINE_INCLUDE_DIR}/readline)
            add_definitions(-DUSE_HISTORY_H)
        endif (NOT READLINE_FOUND)
    endif (${ENHANCE_TERM} STREQUAL "Readline")

    # if we haven't found readline or where asked to link with libedit -- use our custom libedit
    # disable it on Windows (doesn't exist) and FreeBSD (compilation problems) systems though
    if (NOT TERM_LIB AND ${ENHANCE_TERM} STREQUAL "Libedit")
        if (${CMAKE_SYSTEM_NAME} MATCHES "Windows" OR ${CMAKE_SYSTEM_NAME} MATCHES "FreeBSD")
            message(STATUS "Disabling libedit on ${CMAKE_SYSTEM_NAME}")
            message(STATUS "Terminal will not be enhanced")
        else (${CMAKE_SYSTEM_NAME} MATCHES "Windows" OR ${CMAKE_SYSTEM_NAME} MATCHES "FreeBSD")
            find_library(CURSES_LIB curses)
            if (NOT CURSES_LIB)
                message(STATUS "Cannot find libcurses -- disabling libedit")
            else (NOT CURSES_LIB)
                # import libedit library
                add_library(libedit STATIC IMPORTED)
                set_target_properties(libedit PROPERTIES
                    IMPORTED_LOCATION ${CUSTOM_LIBEDIT_LIB}/libedit${CMAKE_STATIC_LIBRARY_SUFFIX})
                include_directories(${CUSTOM_LIBEDIT_INCLUDE})
                set(TERM_LIB libedit curses) # we need libcurses since libedit will not work without it
            endif (NOT CURSES_LIB)
        endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows" OR ${CMAKE_SYSTEM_NAME} MATCHES "FreeBSD")
    elseif (NOT TERM_LIB)
        message(STATUS "Unknown option in ENHANCE_TERM -- terminal will not be enhanced")
    endif (NOT TERM_LIB AND ${ENHANCE_TERM} STREQUAL "Libedit")
endif (NOT ${ENHANCE_TERM} STREQUAL "None")

set(term_SRC term_funcs.cpp sprompt.cpp startup.cpp mainloop.cpp term_ile.cpp complet.cpp)
SET_SOURCES(term_SRC 1)

if (TERM_LIB)
    add_definitions(-DENABLE_LIBEDIT)
endif (TERM_LIB)

# for now we'll link terminal with the static driver, since we don't have install
if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    if (MS_LINK_RUNTIME_STATIC)
        set(TERM_SEDNA_LIB sedna_mt)
    else (MS_LINK_RUNTIME_STATIC)
        set(TERM_SEDNA_LIB sedna_md)
    endif (MS_LINK_RUNTIME_STATIC)
else (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(TERM_SEDNA_LIB sedna_npic)
endif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")

add_executable(se_term ${term_SRC})
target_link_libraries(se_term ${TERM_LIB} common ${TERM_SEDNA_LIB} expat ${sysaux_LIB})
ConfigOutputDir(se_term)

if (TERM_LIB AND ${ENHANCE_TERM} STREQUAL "Libedit")
    add_dependencies(se_term custom_libedit)
endif (TERM_LIB AND ${ENHANCE_TERM} STREQUAL "Libedit")

# install
install(TARGETS se_term DESTINATION bin)
