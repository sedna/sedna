/*
 * File:  sp_defs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SP_DEFS_H
#define _SP_DEFS_H



#define SE_MAX_LOGIN_LENGTH                                511
#define SE_MAX_PASSWORD_LENGTH                             511
#define SE_MAX_DB_NAME_LENGTH                              511
#define SE_MAX_DOCUMENT_NAME_LENGTH                        511
#define SE_MAX_COLLECTION_NAME_LENGTH                      511
#define SE_MAX_DIR_LENGTH                                  255
#define SE_HOSTNAMELENGTH                                  255

#define SE_SOCKET_MSG_BUF_SIZE                             10240
#define SE_MAX_QUERY_SIZE                                  2097152 // Maximum query size 2 Mb

#define SE_CURRENT_SOCKET_PROTOCOL_VERSION_MAJOR           3
#define SE_CURRENT_SOCKET_PROTOCOL_VERSION_MINOR           0

#define SEDNA_DEBUG_OFF                                    0
#define SEDNA_DEBUG_ON                                     1

typedef int sp_int32;

enum se_debug_info_type 
{
    se_QueryTrace,
    se_QueryDebug
};

enum se_sp_instructions
{ se_ErrorResponse = 100,
    se_StartUp = 110,
    se_SessionParameters = 120,
    se_AuthenticationParameters = 130,
    se_SendSessionParameters = 140,
    se_SendAuthParameters = 150,
    se_AuthenticationOK = 160,
    se_AuthenticationFailed = 170,
    se_BeginTransaction = 210,
    se_CommitTransaction = 220,
    se_RollbackTransaction = 225,
    se_BeginTransactionOk = 230,
    se_BeginTransactionFailed = 240,
    se_CommitTransactionOk = 250,
    se_CommitTransactionFailed = 260,
    se_RollbackTransactionOk = 255,
    se_RollbackTransactionFailed = 265,
    se_Execute = 300,
    se_ExecuteLong = 301,
    se_LongQueryEnd = 302,
    se_GetNextItem = 310,
    se_QuerySucceeded = 320,
    se_DebugInfo = 325,
    se_QueryFailed = 330,
    se_UpdateSucceeded = 340,
    se_UpdateFailed = 350,
    se_ItemPart = 360,
    se_ItemEnd = 370,
    se_ResultEnd = 375,
    se_BulkLoadError = 400,
    se_BulkLoadPortion = 410,
    se_BulkLoadEnd = 420,
    se_BulkLoadFileName = 430,
    se_BulkLoadFromStream = 431,
    se_BulkLoadSucceeded = 440,
    se_BulkLoadFailed = 450,
    se_ShowTime = 451,
    se_LastQueryTime = 452,
    se_CloseConnection = 500,
    se_CloseConnectionOk = 510,
    se_TransactionRollbackBeforeClose = 520,
    se_Authenticate = 90,
    se_ExecuteSchemeProgram = 95,
    se_SetSessionOptions = 530,
    se_SetSessionOptionsOk = 540,
    se_ResetSessionOptions = 550,
    se_ResetSessionOptionsOk = 560
};

struct msg_struct
{
    sp_int32 instruction;
    sp_int32 length;
    char body[SE_SOCKET_MSG_BUF_SIZE];
};

struct protocol_version{
	char major_version;
	char minor_version;
};

#endif
