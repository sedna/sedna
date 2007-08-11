#if _MSC_VER > 1000
#pragma once
#endif

#ifndef WUPERFLOG_INCLUDED
#define WUPERFLOG_INCLUDED

#define WUPERF_EVENT_BEGIN		0
#define WUPERF_EVENT_END		1
#define WUPERF_EVENT_ATOMIC		2

/* Sm calls from trn */ 

#define WUTOK_SM_CALLED_BY_TRN            ( 0)
#define WUTOK_REGISTER_TRANSACTION_CALL   ( 1)
#define WUTOK_UNREGISTER_TRANSACTION_CALL ( 2)
#define WUTOK_COMMIT_TRANSACTION_CALL     ( 3)
#define WUTOK_ROLLBACK_TRANSACTION_CALL   ( 4)
#define WUTOK_GET_BLOCK_CALL              ( 5)
#define WUTOK_CREATE_BLOCK_VERSION_CALL   ( 6)
#define WUTOK_ALLOCATE_DATA_BLOCK_CALL    ( 7)
#define WUTOK_ALLOCATE_TEMP_BLOCK_CALL    ( 8)
#define WUTOK_DELETE_BLOCK_CALL           ( 9)

/* Primitive HDD io procedures */ 

#define WUTOK_EXTEND_DATA_FILE            (10)
#define WUTOK_EXTEND_TEMP_FILE            (11)
#define WUTOK_READ_BLOCK                  (12)
#define WUTOK_WRITE_BLOCK                 (13)

/* Buffer management procedures */ 

#define WUTOK_PUT_BLOCK_TO_BUFFER         (14)
#define WUTOK_GET_FREE_BUFFER             (15)
#define WUTOK_VMM_CALLBACK                (16)
#define WUTOK_FLUSH_BUFFER                (17)
#define WUTOK_FLUSH_BUFFERS               (18)

/* Allocation procedures */ 

#define WUTOK_ALLOCATE_DATA_BLOCK         (19)
#define WUTOK_DELETE_DATA_BLOCK           (20)
#define WUTOK_ALLOCATE_TEMP_BLOCK         (21)
#define WUTOK_DELETE_TEMP_BLOCK           (22)

/* No more tokens currently defined */ 

int WuPerfLogInitialize(const char *filename);
int WuPerfLogDeinitialize();
int WuPerfLogRecordEvent(int type, int token, const char *fmt, ...);

#endif
