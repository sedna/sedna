#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "wuperflog.h"
#include "wuerr.h"

#define PL_BUF_SZ		1024
#define PL_MAX_DEPTH	128

#if defined(__GNUC__) && (__GNUC__ >= 4) && (__GNUC_MINOR__ >= 2)
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif /* GNUC */

static const size_t stringTableSize = 23;
static const char *stringTable[stringTableSize] = 
{
	"SM_CALLED_BY_TRN", 
	"REGISTER_TRANSACTION_CALL", 
	"UNREGISTER_TRANSACTION_CALL", 
	"COMMIT_TRANSACTION_CALL", 
	"ROLLBACK_TRANSACTION_CALL", 
	"GET_BLOCK_CALL", 
	"CREATE_BLOCK_VERSION_CALL", 
	"ALLOCATE_DATA_BLOCK_CALL", 
	"ALLOCATE_TEMP_BLOCK_CALL", 
	"DELETE_BLOCK_CALL", 
	"EXTEND_DATA_FILE", 
	"EXTEND_TEMP_FILE", 
	"READ_BLOCK", 
	"WRITE_BLOCK", 
	"PUT_BLOCK_TO_BUFFER", 
	"FREE_BUFFER", 
	"VMM_CALLBACK", 
	"FLUSH_BUFFER", 
	"FLUSH_BUFFERS", 
	"ALLOCATE_DATA_BLOCK", 
	"DELETE_DATA_BLOCK", 
	"ALLOCATE_TEMP_BLOCK", 
	"DELETE_TEMP_BLOCK"
};

struct WuPerfRecord
{
	int token, duration, tick;
	WuPerfRecord *children, *next;
	const char *description;
	size_t descriptLen;
};

struct WuPerfContext
{
	WuPerfRecord root, *currentNode, *upTrace[PL_MAX_DEPTH];
	int depth;
};

static
void InitPerfRecord(WuPerfRecord *rec)
{
	assert(rec);
	rec->token=-1;
	rec->tick=0;
	rec->children=NULL;
	rec->next=NULL;
	rec->description=NULL;
}

static
WuPerfRecord *AllocatePerfRecord()
{
	WuPerfRecord *result = NULL;
	result = (WuPerfRecord *)malloc(sizeof *result);
	if (result)
	{
		InitPerfRecord(result);
	}
	return result;
}

static
void DeletePerfRecord(WuPerfRecord *rec)
{
	assert(rec);
	free(rec);
}

static
char *DuplicateString(const char *str, size_t strLen)
{
	char *result = NULL;
	assert(str);
	result = (char *)malloc(strLen+1);
	if (result)
	{
		memcpy(result,str,strLen);
		result[strLen]='\0';
	}
	return result;
}

static
void DeleteString(char *str)
{
	assert(str);
	free(str);
}

static 
int PerfContextInit(WuPerfContext *context)
{
	return 1;
}

static 
int PerfContextFlush(WuPerfContext *context)
{
	return 1;
}

static
int PerfContextAddRecord(WuPerfRecord *record)
{
	return 1;
}

static 
int PerfContextDoneRecord(WuPerfRecord *record)
{
	return 1;
}

static int isInitialized=0;
static int outputType = 0;
static FILE *perfLogFile=NULL;

int WuPerfLogInitialize(const char *filename)
{
	int success = 0;
	perfLogFile = fopen(filename,"wt");
	if (isInitialized)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		if (perfLogFile)
		{
			success=1;
			isInitialized=0;
		}
	}
	if (!success)
	{
		if (perfLogFile) { fclose(perfLogFile); perfLogFile=NULL; }
	}
	return success;
}

int WuPerfLogDeinitialize()
{
	int success = 0;

	if (!isInitialized)
	{
		WuSetLastErrorMacro(WUERR_FUNCTION_INVALID_IN_THIS_STATE);
	}
	else
	{
		isInitialized=0;
		assert(perfLogFile);
		fclose(perfLogFile); perfLogFile=NULL;
		success=1;
	}
	return success;
}

int WuPerfLogRecordEvent(int type, int token, const char *fmt, ...)
{
	return 1;
}
