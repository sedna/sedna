/*
 * File:  sp_defs.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
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

#define SEDNA_READONLY_TRANSACTION                         2
#define SEDNA_UPDATE_TRANSACTION                           3

#define SEDNA_QUERY_EXEC_TIMEOUT                           4
#define SEDNA_MAX_RESULT_SIZE                              5

#define SEDNA_LOG_AMOUNT                                   6
#define SEDNA_LOG_LESS                                     7
#define SEDNA_LOG_FULL                                     8


typedef int sp_int32;

enum se_debug_info_type 
{
    se_QueryTrace,
    se_QueryDebug
};

enum se_item_class
{
    se_atomic    = 1,
    se_document  = 2,
    se_element   = 3,
    se_attribute = 4,
    se_namespace = 5,
    se_pi        = 6,
    se_comment   = 7,
    se_text      = 8
};

enum se_item_type
{
    /* Abstract base types */
    se_anyType            = 0,
    se_anySimpleType      = 1,
    se_anyAtomicType      = 2,
    /* Built-in simple, non-atomic types */
    se_IDREFS             = 3,
    se_NMTOKENS           = 4,
    se_ENTITIES           = 5,
    /* Built-in complex types */
    se_untyped            = 6,
    /* Built-in atomic types (Primitive types) */
    se_dateTime           = 10,
    se_date               = 11,
    se_time               = 12,
    se_duration           = 13,
    se_yearMonthDuration  = 14,
    se_dayTimeDuration    = 15,
    se_gYearMonth         = 16,
    se_gYear              = 17,
    se_gMonthDay          = 18,
    se_gDay               = 19,
    se_gMonth             = 20,
    se_float              = 21,
    se_double             = 22,
    se_decimal            = 23,
    se_integer            = 24,
    se_boolean            = 25,
    se_untypedAtomic      = 26,
    se_string             = 27,
    se_base64Binary       = 28,
    se_hexBinary          = 29,
    se_anyURI             = 30,
    se_QName              = 31,
    se_NOTATION           = 32,
    /* Types derived from xs:string */
    se_normalizedString   = 41,
    se_token              = 42,
    se_language           = 43,
    se_NMTOKEN            = 44,
    se_Name               = 45,
    se_NCName             = 46,
    se_ID                 = 47,
    se_IDREF              = 48,
    se_ENTITY             = 49,
    /* Types derived from xs:integer */
    se_nonPositiveInteger = 50,
    se_negativeInteger    = 51,
    se_long               = 52,
    se_int                = 53,
    se_short              = 54,
    se_byte               = 55,
    se_nonNegativeInteger = 56,
    se_unsignedLong       = 57,
    se_unsignedInt        = 58,
    se_unsignedShort      = 59,
    se_unsignedByte       = 60,
    se_positiveInteger    = 61
};


enum se_sp_instructions
{   
    se_ErrorResponse = 100,
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
    se_ItemStart = 355,
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


#endif /* _SP_DEFS_H */
