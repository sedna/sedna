if(COMMAND cmake_policy)
    cmake_policy(SET CMP0003 NEW)
        cmake_policy(SET CMP0015 NEW)
endif(COMMAND cmake_policy)

OPTION( HAVE_GETOPT_H "Have the getopt library" 0 )
IF( HAVE_GETOPT_H )
        ADD_DEFINITIONS( -DHAVE_GETOPT_H )
ENDIF( HAVE_GETOPT_H )

OPTION( HAVE_INTTYPES_H "Have the file inttypes.h" 1 )
IF( HAVE_INTTYPES_H )
        ADD_DEFINITIONS( -DHAVE_INTTYPES_H )
ENDIF( HAVE_INTTYPES_H )

OPTION( HAVE_MEMORY_H "Have the file memory.h" 1 )
IF( HAVE_MEMORY_H )
        ADD_DEFINITIONS( -DHAVE_MEMORY_H )
ENDIF( HAVE_MEMORY_H )

OPTION( HAVE_STDLIB_H "Have the file stdlib.h" 1 )
IF( HAVE_STDLIB_H )
        ADD_DEFINITIONS( -DHAVE_STDLIB_H )
ENDIF( HAVE_STDLIB_H )

OPTION( HAVE_STRINGS_H "Have the strings.h" 0 )
IF( HAVE_STRINGS_H )
        ADD_DEFINITIONS( -DHAVE_STRINGS_H )
ENDIF( HAVE_STRINGS_H )

OPTION( HAVE_STRING_H "Have the file string.h" 1 )
IF( HAVE_STRING_H )
        ADD_DEFINITIONS( -DHAVE_STRING_H )
ENDIF( HAVE_STRING_H )

OPTION( HAVE_STDC_HEADERS "Have std C header files" 1 )
IF( HAVE_STDC_HEADERS )
        ADD_DEFINITIONS( -DSTDC_HEADERS )
ENDIF( HAVE_STDC_HEADERS )

IF( WIN32 )
  set( argtable2_SRC arg_dbl.c arg_end.c arg_file.c arg_int.c arg_lit.c arg_rem.c 
      arg_str.c argtable2.c argtable2.h getopt.c getopt.h getopt1.c )
ELSE( WIN32 )
  set( argtable2_SRC arg_date.c arg_dbl.c arg_end.c arg_file.c arg_int.c arg_lit.c 
      arg_rem.c arg_rex.c arg_str.c argtable2.c argtable2.h getopt.c getopt.h getopt1.c )
ENDIF( WIN32 )