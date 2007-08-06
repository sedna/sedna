/* GENERATED FILE, DO NOT EDIT! */ 
#if (_MSC_VER > 1000)
#pragma once
#endif
#ifndef WUERR_INCLUDED
#define WUERR_INCLUDED

#ifndef WUERR_FIRST_ERR
#define WUERR_FIRST_ERR 0
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
#define WUERR_NOT_IMPLEMENTED  											 (36 + WUERR_FIRST_ERR)
#define WUERR_UNKNOWN_EXCEPTION											 (37 + WUERR_FIRST_ERR)

/* state table errors */ 

#define WUERR_BAD_TICKET                                                 ( 8 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_BAD_ROW_ID                                     ( 9 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_FULL                                           (10 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_MAX_NUMBER_OF_COLUMNS_EXCEEDED                 (11 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_MAX_ROW_SIZE_EXCEEDED                          (12 + WUERR_FIRST_ERR)
#define WUERR_STATE_TABLE_MAX_NUMBER_OF_COLUMNS_WITH_DEBUG_INFO_EXCEEDED (13 + WUERR_FIRST_ERR)

/* clients errors */ 

#define WUERR_BAD_CLIENT_ID                                              (14 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_ID_ALREADY_IN_USE                                   (15 + WUERR_FIRST_ERR)
#define WUERR_MAX_NUMBER_OF_CLIENTS_EXCEEDED                             (16 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_ALREADY_MARKED_READY                                (17 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_ALREADY_MARKED_LEAVING                              (18 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_SET_ALREADY_UNLOCKED                                (19 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_SET_MAX_NUMBER_OF_LOCKS_EXCEEDED                    (20 + WUERR_FIRST_ERR)
#define WUERR_CLIENT_SET_DEADLOCK_DETECTED                               (21 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_UNREGISTER_CURRENT_CLIENT                        (22 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_UNREGISTER_READY_CLIENT                          (23 + WUERR_FIRST_ERR)

/* snapshots errors */ 

#define WUERR_UNABLE_TO_DISCARD_SNAPSHOT_IN_USE                          (24 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_DISCARD_SPECIAL_SNAPSHOT                         (25 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_USE_DAMAGED_SNAPSHOT                             (36 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_DAMAGE_SNAPSHOT_IN_USE                           (26 + WUERR_FIRST_ERR)
#define WUERR_SNAPSHOT_ALREADY_PERSISTENT                                (27 + WUERR_FIRST_ERR)
#define WUERR_MAX_NUMBER_OF_SNAPSHOTS_EXCEEDED                           (28 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOT_WITH_THIS_TIMESTAMP                            (29 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOT_WITH_THIS_TYPE                                 (30 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOT_WITH_THIS_ORDINAL                              (31 + WUERR_FIRST_ERR)
#define WUERR_SNAPSHOT_WITH_THIS_TIMESTAMP_ALREADY_EXISTS                (32 + WUERR_FIRST_ERR)
#define WUERR_UNABLE_TO_ADVANCE_SNAPSHOTS                                (33 + WUERR_FIRST_ERR)
#define WUERR_NO_SNAPSHOTS_EXIST                                         (34 + WUERR_FIRST_ERR)

/* timestamps errors */ 

#define WUERR_BAD_TIMESTAMP                                              (35 + WUERR_FIRST_ERR)

/* versions errors */ 



/* no more errors */ 

#endif
