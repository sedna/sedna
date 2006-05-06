/*
 * File: assert.c
 *
 * Portions Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 * Portions Copyright (c) 1996-2005, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 */


#include "sedna.h"


/*
 * se_ExceptionalCondition - Handles the failure of an SE_ASSERT()
 */
int se_ExceptionalCondition(char *conditionName, char *errorType,
                            char *fileName, int lineNumber)
{
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

#ifdef SE_SLEEP_ON_ASSERT
	/*
	 * Sleep for 10 days. It should be enough to find out the problem.
	 */
	uSleep(8640 * 10, __sys_call_error); 
#endif

    SEDNA_SOFT_FAULT_BASE_MSG;
    SEDNA_SOFT_FAULT_FINALIZER;

	return 0;
}
