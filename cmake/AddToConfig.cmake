# This macros sets definition -DVarName=Value for debug-type and release-type configurations
# mainly needed for VisualStudio-like configurations to work properly -- with must add the definition for all confs at once
macro (AddDefinitionDebRel VarName ValueDeb ValueRes)
    if (CMAKE_CONFIGURATION_TYPES)
        set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS_DEBUG "${VarName}=${ValueDeb}")

        set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS_RELWITHDEBINFO "${VarName}=${ValueRes}")
        set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS_MINSIZEREL "${VarName}=${ValueRes}")
        set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS_RELEASE "${VarName}=${ValueRes}")
    else (CMAKE_CONFIGURATION_TYPES)
        if (${CMAKE_BUILD_TYPE} MATCHES "Debug" OR NOT CMAKE_BUILD_TYPE)
            set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS "${VarName}=${ValueDeb}")
        else (${CMAKE_BUILD_TYPE} MATCHES "Debug" OR NOT CMAKE_BUILD_TYPE)
            set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS "${VarName}=${ValueRes}")
        endif (${CMAKE_BUILD_TYPE} MATCHES "Debug" OR NOT CMAKE_BUILD_TYPE)
    endif (CMAKE_CONFIGURATION_TYPES)
endmacro (AddDefinitionDebRel VarName ValueDeb ValueRes)
