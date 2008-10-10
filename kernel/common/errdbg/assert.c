/*
 * File: assert.c
 *
 * Portions Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * Portions Copyright (c) 1996-2005, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 */


#include "common/sedna.h"
#include "common/errdbg/event_log.h"
#include "common/u/uhdd.h"
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#include "common/st/stacktrace.h"
#ifdef _WIN32
#include <dbghelp.h>
#include <shellapi.h>
#include <shlobj.h>
#endif
#endif

/*
 * se_ExceptionalCondition - Handles the failure of an SE_ASSERT()
 */
int se_ExceptionalCondition(const char *conditionName, const char *errorType,
                            const char *fileName, int lineNumber)
{
	char buf[SEDNA_DATA_VAR_SIZE + 128];
    UFile a_fh;

	if (!PointerIsValid(conditionName)
		|| !PointerIsValid(fileName)
		|| !PointerIsValid(errorType))
        fprintf(stderr, "TRAP: ExceptionalCondition: bad arguments\n");
	else
	{
        fprintf(stderr, "TRAP: %s(\"%s\", File: \"%s\", Line: %d)\n",
                         errorType, conditionName,
                         fileName, lineNumber);
	}



    strcpy(buf, SEDNA_DATA);
#ifdef _WIN32
    strcat(buf, "\\data\\");
#else
    strcat(buf, "/data/");
#endif
    if (uMkDir(buf, NULL, NULL) == 0)
       fprintf(stderr, "Cannot create data directory for soft fault logs\n");   

    strcat(buf, SE_LAST_SOFT_FAULT_DIR);

    if (uMkDir(buf, NULL, NULL) == 0)
       fprintf(stderr, "Cannot create directory for soft fault logs\n");


#ifdef _WIN32
    strcat(buf, "\\");
#else
    strcat(buf, "/");
#endif
    strcat(buf, SE_ASSERT_FAILED_FILE_NAME);

#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
#ifdef _WIN32
	strcat(buf, ".dmp");
    a_fh = uCreateFile(buf, U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH, NULL, NULL);
    if (a_fh == U_INVALID_FD)
        fprintf(stderr, "Can't create assert_failed.dmp file\n");
	else
	{
		MiniDumpWriteDump(GetCurrentProcess(), GetCurrentProcessId(), a_fh, MiniDumpWithDataSegs, NULL, NULL, NULL);

		uCloseFile(a_fh, NULL);
	}
	buf[strlen(buf)-4] = 0;
#endif
#endif

    a_fh = uCreateFile(buf, U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, U_WRITE_THROUGH, NULL, NULL);
    if (a_fh == U_INVALID_FD)
        fprintf(stderr, "Can't create assert_failed file\n");
	else
	{    
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
		if (StackTraceInit() != 0)
		{
			StackTraceWriteFd(NULL, (intptr_t)a_fh, 9999, 1);
			
			StackTraceDeinit();
		}
		else
			fprintf(stderr, "StackTraceInit() failed\n");
#endif
		uCloseFile(a_fh, NULL);
	}

#ifdef SE_SLEEP_ON_ASSERT
	/*
	 * Sleep for 10 days. It should be enough to find out the problem.
	 */
	uSleep(8640 * 10, __sys_call_error); 
#endif

    sedna_soft_fault(EL_UNK);

	return 0;
}
