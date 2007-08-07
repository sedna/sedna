/* GENERATED FILE, DO NOT EDIT! */ 
#if (_MSC_VER > 1000)
#pragma once
#endif
#ifndef WUERR_INCLUDED
#define WUERR_INCLUDED

#ifndef WUERR_FIRST_ERR
#define WUERR_FIRST_ERR 0x20000000
#endif

/* generic errors */ 

#define WUERR_GENERAL_ERROR                                              ( 0 + WUERR_FIRST_ERR)
#define WUERR_BAD_PARAMS                                                 ( 1 + WUERR_FIRST_ERR)
#define WUERR_FUNCTION_INVALID_IN_THIS_STATE                             ( 2 + WUERR_FIRST_ERR)
#define WUERR_STARTUP_ERROR                                              ( 3 + WUERR_FIRST_ERR)
#define WUERR_SHUTDOWN_ERROR                                             ( 4 + WUERR_FIRST_ERR)
#define WUERR_PERS_DATA_VALIDATION_FAILED                                ( 5 + WUERR_FIRST_ERR)
#define WUERR_DEBUG_FUNCTION_UNAVAILABLE                                 ( 6 + WUERR_FIRST_ERR)
#define WUERR_NO_MEMORY                                                  ( 7 + WUERR_FIRST_ERR)
#define WUERR_NOT_IMPLEMENTED                                            ( 8 + WUERR_FIRST_ERR)

/* exceptions - evil! */ 

#define WUERR_UNKNOWN_EXCEPTION                                          ( 9 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_EXCEPTION                                            (10 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_SYSTEM_EXCEPTION                                     (11 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_SYSTEM_ENV_EXCEPTION                                 (12 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_USER_EXCEPTION                                       (13 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_USER_FN_ERROR_EXCEPTION                              (14 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_USER_ENV_EXCEPTION                                   (15 + WUERR_FIRST_ERR)
#define WUERR_SEDNA_USER_SOFT_EXCEPTION                                  (16 + WUERR_FIRST_ERR)

/* state table errors */ 

#define WUERR_BAD_TICKET                                                 (17 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_BAD_ROW_ID                                     (18 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_FULL                                           (19 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_MAX_NUMBER_OF_COLUMNS_EXCEEDED                 (20 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_MAX_ROW_SIZE_EXCEEDED                          (21 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_MAX_NUMBER_OF_COLUMNS_WITH_DEBUG_INFO_EXCEEDED (22 + WUERR_FIRST_ERR)

/* clients errors */ 

#define WUERR_BAD_CLIENT_ID                                              (23 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_ID_ALREADY_IN_USE                                   (24 + WUERR_FIRST_ERR)
#define WUERR_MAX_NUMBER_OF_CLIENTS_EXCEEDED                             (25 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_ALREADY_MARKED_READY                                (26 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_ALREADY_MARKED_LEAVING                              (27 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_SET_ALREADY_UNLOCKED                                (28 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_SET_MAX_NUMBER_OF_LOCKS_EXCEEDED                    (29 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_SET_DEADLOCK_DETECTED                               (30 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_UNREGISTER_CURRENT_CLIENT                        (31 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_UNREGISTER_READY_CLIENT                          (32 + WUERR_FIRST_ERR)

/* snapshots errors */ 

#define WUERR_UNABLE_TO_DISCARD_SNAPSHOT_IN_USE                          (33 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_DISCARD_SPECIAL_SNAPSHOT                         (34 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_USE_DAMAGED_SNAPSHOT                             (35 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_DAMAGE_SNAPSHOT_IN_USE                           (36 + WUERR_FIRST_ERR)
#define WUERR_SNAPSHOT_ALREADY_PERSISTENT                                (37 + WUERR_FIRST_ERR)
#define WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED                           (38 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP                            (39 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOT_WITH_THIS_TYPE                                 (40 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOT_WITH_THIS_ORDINAL                              (41 + WUERR_FIRST_ERR)
#define WUERR_SNAPSHOT_WITH_THIS_TIMESTAMP_ALREADY_EXISTS                (42 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_ADVANCE_SNAPSHOTS                                (43 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOTS_EXIST                                         (44 + WUERR_FIRST_ERR)

/* timestamps errors */ 

#define WUERR_BAD_TIMESTAMP                                              (45 + WUERR_FIRST_ERR)

/* no more errors */ 

int WuIsAppError(int error);

void WuSetLastError(int error);

void WuSetLastError2(const char *file, int line, const char *function, int error);

#define WuSetLastErrorMacro(error)\
	WuSetLastError2(__FILE__,__LINE__,__FUNCTION__,error)

int WuGetLastError();

int WuPushLastError();

int WuPopLastError();

struct WuErrorProperties
{
	int error;
	const char *description;
	const char *file;
	int line;
	const char *function;
};

void WuGetLastErrorProperties(WuErrorProperties *errorProperties);

#endif
