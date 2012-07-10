/*
 * File:  base.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _BASE_H
#define _BASE_H

#include "u/u.h"

#define SEDNA_DATA_STRUCTURES_VER 12

// buffer memory offset; this type is used for addressing buffers in buffer
// memory area by defining offset of buffer from the beginning of the shared
// memory
typedef size_t ramoffs;
#define RAMOFFS_OUT_OFF_BOUNDS ((size_t)-1)

typedef uint32_t CP_counter;
#define  NULL_FILE (-1)

#define MAX_FILE_SIZE_WITHOUT_CHECKPOINT 50*(1024*1024)

#define SEDNA_NAMESPACE_URI     "http://www.sedna.org/"
//#define SEDNA_NAMESPACE_URI     "http://www.modis.ispras.ru/sedna"
#define SEDNA_NAMESPACE_PREFIX  "se"

typedef int32_t session_id;

/**
 * Transaction identifier
 */
typedef int transaction_id;
#define INVALID_TRID ((transaction_id)-1)

#define MAX_RESOURCE_NAME_LENGTH                100
#define MAX_DATABASE_NAME_LENGTH                100

#define INVALID_SIZE ((size_t)-1)

/* shift in the block */
typedef uint16_t shft;

/*
 * Our page constants.
 *
 * It's hard to define cross-platform constants for 32- and 64- bits architectures,
 * since, for example, 'long' type is 32-bit on win-64. So, we're just using
 * explicit casting to int-pointer type.
 */
#define PAGE_BIT_SIZE                           16
#define PAGE_SIZE                               (1 << PAGE_BIT_SIZE)
#define PAGE_REVERSE_BIT_MASK                   (uintptr_t)(PAGE_SIZE-1)
#define PAGE_BIT_MASK                           (~PAGE_REVERSE_BIT_MASK)

/* For those, who look for: VMM_REGION_* is now in vmm.cpp */

#define VMM_REGION_MIN_SIZE                             ((uint32_t)0x4000000)

#define TR_AUTHENTICATION_FLAG 1
#define TR_AUTHORIZATION_FLAG  2

#ifdef _WIN32
#define SESSION_EXE "se_trn.exe"
#define GOV_EXE "se_gov.exe"
#else
#define SESSION_EXE "se_trn"
#define GOV_EXE "se_gov"
#endif

/*
 * SEDNA_DATA contains path to the Sedna's data directory
 */
#define SEDNA_DATA_VAR_SIZE 1024
extern const char * SEDNA_DATA;

#define MODULES_COLLECTION_NAME "$modules"

#define TRIGGER_MAX_CASCADING_LEVEL                     10

#define SM_NUMBER_OF_SERVER_THREADS                     1

#define GOV_NUMBER_OF_SERVER_THREADS                    1

#define SEDNA_DETERMINE_VMM_REGION                      "SEDNA_DETERMINE_VMM_REGION"
#define CONNECTION_SOCKET_HANDLE                        "CONNECTION_SOCKET_HANDLE"
#define SEDNA_SERVER_MODE                               "SEDNA_SERVER_MODE"

#define SEDNA_LOAD_METADATA_TRANSACTION                 "SEDNA_LOAD_METADATA_TRANSACTION"

#define SEDNA_OS_PRIMITIVES_ID_MIN_BOUND                "SEDNA_OS_PRIMITIVES_ID_MIN_BOUND"

#define SEDNA_RUN_RECOVERY_TRANSACTION                  "SEDNA_RUN_RECOVERY_TRANSACTION"

#define SECURITY_METADATA_DOCUMENT                      "$db_security_data"
#define INITIAL_SECURITY_METADATA_FILE_NAME             "sedna_auth_md.xml"

#define CHARISMA_MAX_TRNS_NUMBER                        50
#define MAX_SESSIONS_NUMBER                             50
#define MAX_DBS_NUMBER                                  10

#define STRMAXSIZE                                      4000000000lu

#define UPPER_SESSIONS_NUM_BOUND                        100

void check_db_name_validness(const char* name);

extern FILE* res_os;

/* The following parameters are related to kernel<-->transaction protocol */
#define  ERR_SYMBOL     ((char)254)
#define  DELIM_SYMBOL   ((char)250)
#define  EOD_SYMBOL     ((char)253)
#define  EOALL_SYMBOL   ((char)252)


/* The following definitions are related to gov<-->rc protocol */
#define  SE_RC_VALID     ((char)1)
#define  SE_RC_INVALID   ((char)0)
#define  SE_RC_OVERFLOW  ((char)2)

#endif
