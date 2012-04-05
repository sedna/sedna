/*
 * File:  usem.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "u/usem.h"
#include "u/uutils.h"
#include "common/errdbg/d_printf.h"
#include "u/ugnames.h"

///////////////////////////////////////////////////////////////////////////////
// Semaphore implementation
///////////////////////////////////////////////////////////////////////////////

int USemaphoreCreate(USemaphore *sem, int init_value, int max_value, global_name name, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
#ifdef _WIN32
{
	char buf[128];
	const char *wName = UWinIPCNameFromGlobalName(name,buf,128);

    *sem = CreateSemaphore(sa, init_value, max_value, wName);

    if (*sem == NULL)
    {
        sys_call_error("CreateSemaphore");
        return 1;
    }
    else
    {
        if (GetLastError() == ERROR_ALREADY_EXISTS)
        {
            d_printf1("CreateSemaphore: already exists\n");
            return 1;
        }	
    }

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(name, "SEM", sem, *sem, 0); };

    return 0;
}
#else
{
	union {
	    int val;
	    struct semid_sd *buf;
	    ushort *array;
	} semctl_arg;
	key_t key = IPC_PRIVATE;

	key = USys5IPCKeyFromGlobalName(name);

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCleanup(name, "SEM", sem, *sem, 0); };

    USECURITY_ATTRIBUTES sem_access_mode = U_SEDNA_SEMAPHORE_ACCESS_PERMISSIONS_MASK;
    if (sa) sem_access_mode = *sa;
    int res = semget(key, 1, IPC_CREAT | IPC_EXCL | sem_access_mode);
    *sem = res;

    if (*sem < 0)
    {
        sys_call_error("semget");
        return 1;
    }
	semctl_arg.val = init_value;

	if(semctl(*sem, 0, SETVAL, semctl_arg) < 0)
	{
        sys_call_error("semctl");
		return 1;
	}

	if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(name, "SEM", sem, *sem, 0); };

    return 0;
}
#endif


int USemaphoreOpen(USemaphore *sem, global_name name, sys_call_error_fun fun)
#ifdef _WIN32
{
	char buf[128];
	const char *wName = UWinIPCNameFromGlobalName(name,buf,128);
    *sem = OpenSemaphore(SEMAPHORE_ALL_ACCESS, FALSE, wName);

    if (*sem == NULL)
    {
        sys_call_error("OpenSemaphore");
        return 1;
    }

    return 0;
}
#else
{
	key_t key = IPC_PRIVATE;
	key = USys5IPCKeyFromGlobalName(name);

    *sem = semget(key, 1, 0);

    if (*sem < 0)
    {
        sys_call_error("semget");
        return 1;
    }

    return 0;
}
#endif

int USemaphoreRelease(USemaphore sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    BOOL res =  CloseHandle(sem);

    if (res == 0)
    {
        sys_call_error("CloseHandle");
        return 1;
    }

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(NULL, "SEM", &sem, sem, 0); };

    return 0;
}
#else
{
/*    union {
	int val;
	struct semid_sd *buf;
	ushort *array;
    } semctl_arg;*/

    if (sem < 0)
    {
        return 1;
    }
	else
	{
		if (semctl(sem, 0, IPC_RMID) < 0)
		{
            sys_call_error("semctl");
			return 1;
		}
	}

	if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(NULL, "SEM", &sem, sem, 0); };

    return 0;
}
#endif


int USemaphoreClose(USemaphore sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    BOOL res =  CloseHandle(sem);

    if (res == 0)
    {
        sys_call_error("CloseHandle");
        return 1;
    }

    return 0;
}
#else
{
    return 0;
}
#endif

int USemaphoreDown(USemaphore sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    DWORD res; 
    res = WaitForSingleObject(sem, INFINITE);
    if (res == WAIT_FAILED || res != WAIT_OBJECT_0)
    {
        sys_call_error("WaitForSingleObject");
        return 1;
    }

    return 0;
}
#else
{
    int res;
	
    //struct sembuf op_op[1] = {{0, -1, 0}};
    struct sembuf op_op[1];
    op_op[0].sem_num = 0;
    op_op[0].sem_op = -1;
    op_op[0].sem_flg = 0;


    while (true)
    {
        res = semop(sem, op_op, 1);
	
        if (res == 0) return 0;
        else if (errno == EINTR) continue;
        else 
        {
            sys_call_error("semop");
            return 1;
        }
    }
}
#endif


int USemaphoreDownTimeout(USemaphore sem, unsigned int millisec, sys_call_error_fun fun)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(sem, millisec);

    if (res == WAIT_FAILED)
    {
        sys_call_error("WaitForSingleObject");
        return 1;
    }
 
    if (res == WAIT_OBJECT_0) return 0;

    if (res == WAIT_TIMEOUT) return 2;

    return 1;
}
#else
{
    int res = 0;
    
    struct sembuf op_op[1];
    op_op[0].sem_num = 0;
    op_op[0].sem_op = -1;
    op_op[0].sem_flg = IPC_NOWAIT;

    do
    {    
    	res = semop(sem, op_op, 1);
	
    	if (res == 0)
    	{
    	    return 0;
    	}
        else if (errno == EAGAIN)
        {
            if (millisec >= 1000)
            {
                sleep(1);
                millisec -= 1000;
            }
            else
            {
                /* we should do something here instead of break;
                 * for example:
                 *
                 * nanosleep(...);
                 */
                millisec = 0;
            }
        }
        else 
        {
            sys_call_error("semop");
            return 1;
        }
    } while (millisec);

    return 2;
} 
#endif

int USemaphoreUp(USemaphore sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    BOOL res;

    res = ReleaseSemaphore(sem, 1, NULL);

    if (res == 0) 
    {
        sys_call_error("ReleaseSemaphore");
        return 1;
    }

    return 0;
}
#else
{
	int res;
	
	//struct sembuf op_op[1] = {{0, 1, IPC_NOWAIT}};
    struct sembuf op_op[1];
    op_op[0].sem_num = 0;
    op_op[0].sem_op = 1;
    op_op[0].sem_flg = IPC_NOWAIT;

	res = semop(sem, op_op, 1);	
	if(res < 0)
	{
        sys_call_error("semop");
		return 1;	
	}
	return 0;
}
#endif






///////////////////////////////////////////////////////////////////////////////
// Array of semaphore implementation
///////////////////////////////////////////////////////////////////////////////

#define SIZE_OF_BUF_FOR_ADJUSTED_NAME 128

int USemaphoreArrCreate(USemaphoreArr *sem, unsigned size, const int *init_values, global_name name, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
#ifdef _WIN32
{
    unsigned i = 0;
    char buf[128];
	const char *wName = NULL;
    size_t name_len = 0;

	wName = UWinIPCNameFromGlobalName(name,buf,128);
	if (wName) name_len = strlen(wName);

    /*
     * We concatenate name with a number and put the result to buf. 
     * Because int2c_str requieres 20 bytes the length of the name
     * must not be greater than (SIZE_OF_BUF_FOR_ADJUSTED_NAME - 20)
     */
    *sem = (USemaphoreArr)malloc(sizeof(HANDLE) * size);

    for (i = 0; i < size; i++)
    {
		sprintf(buf+name_len,":%u",i);

        (*sem)[i] = CreateSemaphore(sa,
                                    init_values[i],
                                    INT_MAX,
                                    wName);

        if ((*sem)[i] == NULL)
        {
            free(*sem);
            sys_call_error("CreateSemaphore");
            return 1;
        }
        else
        {
            if (GetLastError() == ERROR_ALREADY_EXISTS)
            {
                free(*sem);
                d_printf1("CreateSemaphore: already exists\n");
                return 1;
            }	
        }
    }

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(name, "WSEA", sem, *sem, size); };

    return 0;
}
#else
{
	union {
	    int val;
	    struct semid_sd *buf;
	    ushort *array;
	} semctl_arg;
	key_t key = IPC_PRIVATE;
    unsigned i = 0;

	key = USys5IPCKeyFromGlobalName(name);

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCleanup(name, "SEA", sem, *sem, size); };

    USECURITY_ATTRIBUTES sem_access_mode = U_SEDNA_SEMAPHORE_ACCESS_PERMISSIONS_MASK;
    if (sa) sem_access_mode = *sa;
    *sem = semget(key, (int)size, IPC_CREAT | IPC_EXCL | sem_access_mode);

    if (*sem < 0)
    {
        sys_call_error("semget");
        return 1;
    }

    for (i = 0; i < size; i++)
    {
	    semctl_arg.val = init_values[i];

        if(semctl(*sem, i, SETVAL, semctl_arg) < 0)
        {
		    //d_printf3("ERRROR: %d. Can't set initial value for %x semaphore\n", perror(semctl), (int)name);
		    //d_printf2("Can't set initial value for %x semaphore\n", (int)name);
            sys_call_error("semctl");
		    return 1;
    	}
    }

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(name, "SEA", sem, *sem, size); };

    return 0;
}
#endif

int USemaphoreArrOpen(USemaphoreArr *sem, unsigned size, global_name name, sys_call_error_fun fun)
#ifdef _WIN32
{
    unsigned i = 0;
    char buf[128];
    size_t name_len = 0;
	const char *wName = NULL;

	wName = UWinIPCNameFromGlobalName(name,buf,128);
	if (wName) name_len = strlen(wName);

    *sem = (USemaphoreArr)malloc(sizeof(HANDLE) * size);

    for (i = 0; i < size; i++)
    {
		sprintf(buf+name_len,":%u",i);

        (*sem)[i] = OpenSemaphore(SEMAPHORE_ALL_ACCESS, 
                                  FALSE, 
                                  wName);

        if ((*sem)[i] == NULL)
        {
            free(*sem);
            sys_call_error("OpenSemaphore");
            return 1;
        }
    }

    return 0;
}
#else
{
	key_t key = IPC_PRIVATE;
	key = USys5IPCKeyFromGlobalName(name);

    *sem = semget(key, (int)size, 0);

    if (*sem < 0)
    {
        sys_call_error("semget"); 
        return 1;
    }

    return 0;
}
#endif


int USemaphoreArrRelease(USemaphoreArr sem, unsigned size, sys_call_error_fun fun)
#ifdef _WIN32
{
    unsigned i = 0;
    BOOL res;

    for (i = 0; i < size; i++)
    {
        res = CloseHandle(sem[i]);

        if (res == 0)
        {
            sys_call_error("CloseHandle");
            return 1;
        }
    }

    free(sem);

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(NULL, "SEA", sem, *sem, size); };
    return 0;
}
#else
{
/*    union {
        int val;
        struct semid_sd *buf;
        ushort *array;
    } semctl_arg;*/

    if (sem < 0)
    {
        d_printf1("CloseHandle failed\n");
        //d_printf2("Error %d\n", perror(semget));
        return 1;
    }
	else
	{
		//semctl_arg.val = 0;
		if (semctl(sem, 0, IPC_RMID/*, semctl_arg*/) < 0)
		{
            sys_call_error("semctl");
			return 1;
		}
	}

	if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(NULL, "SEA", &sem, sem, size); };
    return 0;
}
#endif


int USemaphoreArrClose(USemaphoreArr sem, unsigned size, sys_call_error_fun fun)
#ifdef _WIN32
{
    unsigned i = 0;
    BOOL res;

    for (i = 0; i < size; i++)
    {
        res = CloseHandle(sem[i]);

        if (res == 0)
        {
            sys_call_error("CloseHandle");
            return 1;
        }
    }

    free(sem);

    return 0;
}
#else
{
    return 0;
}
#endif

int USemaphoreArrDown(USemaphoreArr sem, unsigned i, sys_call_error_fun fun)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(sem[i], INFINITE);

    if (res == WAIT_FAILED || res != WAIT_OBJECT_0)
    {
        sys_call_error("WaitForSingleObject");
        return 1;
    }

    return 0;
}
#else
{
    int res;
	
    //struct sembuf op_op[1] = {{0, -1, 0}};
    struct sembuf op_op[1];
    op_op[0].sem_num = i;
    op_op[0].sem_op = -1;
    op_op[0].sem_flg = 0;


    while (true)
    {
        res = semop(sem, op_op, 1);
	
        if (res == 0) return 0;
        else if (errno == EINTR) continue;
        else 
        {
            sys_call_error("semop");
            return 1;
        }
    }
}
#endif


int USemaphoreArrDownTimeout(USemaphoreArr sem, unsigned i, unsigned int millisec, sys_call_error_fun fun)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(sem[i], millisec);

    if (res == WAIT_FAILED)
    {
        sys_call_error("WaitForSingleObject");
        return 1;
    }
 
    if (res == WAIT_OBJECT_0) return 0;
    if (res == WAIT_TIMEOUT) return 2;

    return 1;
}
#else
{
    int res = 0;
    unsigned int count = 0;
    
    //struct sembuf op_op[1] = {{0, -1, IPC_NOWAIT}};
    struct sembuf op_op[1];
    op_op[0].sem_num = i;
    op_op[0].sem_op = -1;
    op_op[0].sem_flg = IPC_NOWAIT;

    do
    {    
        res = semop(sem, op_op, 1);

        if (res == 0)
        {
            return 0;
        }
        else if (errno == EAGAIN)
        {
            if (millisec >= 1000)
            {
                sleep(1);
                millisec -= 1000;
            }
            else
            {
                /* we should do something here instead of break;
                 * for example:
                 *
                 * nanosleep(...);
                 */
                millisec = 0;
            }
        }
        else 
        {
            sys_call_error("semop");
            return 1;
        }
    } while (millisec);

    return 2;
} 
#endif

int USemaphoreArrUp(USemaphoreArr sem, unsigned i, sys_call_error_fun fun)
#ifdef _WIN32
{
    BOOL res;

    res = ReleaseSemaphore(sem[i], 1, NULL);

    if (res == 0) 
    {
        sys_call_error("ReleaseSemaphore");
        return 1;
    }

    return 0;
}
#else
{
	int res;
	
	//struct sembuf op_op[1] = {{0, 1, IPC_NOWAIT}};
	struct sembuf op_op[1];
    op_op[0].sem_num = i;
    op_op[0].sem_op = 1;
    op_op[0].sem_flg = IPC_NOWAIT;

	res = semop(sem, op_op, 1);
	
	if(res < 0)
	{
        sys_call_error("semop");
		return 1;	
	}
	return 0;
}
#endif




///////////////////////////////////////////////////////////////////////////////
// Unnamed semaphore implementation
///////////////////////////////////////////////////////////////////////////////

int UUnnamedSemaphoreCreate(UUnnamedSemaphore *sem, int init_value, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
#ifdef _WIN32
{
    *sem = CreateSemaphore(sa, init_value, INT_MAX, NULL);

    if (*sem == NULL)
    {
        sys_call_error("CreateSemaphore");
        return 1;
    }

    return 0;
}
#else
{
    int res = 0;

    res = pthread_mutex_init(&(sem->mutex), NULL);
    if (res != 0)
    {
        sys_call_error("pthread_mutex_init");
        return 1;
    }

    res = pthread_cond_init(&(sem->condition), NULL);
    if (res != 0)
    {
        sys_call_error("pthread_cond_init"); 
        if (pthread_mutex_destroy(&(sem->mutex)) != 0) 
           sys_call_error("pthread_mutex_destroy");
        return 1;
    }

    sem->count = init_value;

    return 0;
}
#endif

int UUnnamedSemaphoreRelease(UUnnamedSemaphore *sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    BOOL res =  CloseHandle(*sem);

    if (res == 0)
    {
        sys_call_error("CloseHandle");
        return 1;
    }

    return 0;
}
#else
{
    int res = 0;
    res = pthread_mutex_destroy(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_destroy");
        return 1;
    }

    res = pthread_cond_destroy(&(sem->condition));
    if (res != 0)
    {
        sys_call_error("pthread_cond_destroy");
        return 1;
    }

    return 0;
}
#endif

int UUnnamedSemaphoreDown(UUnnamedSemaphore *sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(*sem, INFINITE);

    if (res == WAIT_FAILED || res != WAIT_OBJECT_0)
    {
        sys_call_error("WaitForSingleObject");
        return 1;
    }

    return 0;
}
#else
{
    int res = 0;

    res = pthread_mutex_lock(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_lock");
        return 1;
    }

    while (sem->count <= 0)
    {
        res = pthread_cond_wait(&(sem->condition), &(sem->mutex));
        if (res != 0)
        {
            sys_call_error("pthread_cond_wait");
            return 1;
        }
    }

    sem->count--;

    res = pthread_mutex_unlock(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_unlock");
        return 1;
    }

    return 0;
}
#endif

// down with timout
// return values: 0 - success
//                1 - falure
//                2 - timeout
int UUnnamedSemaphoreDownTimeout(UUnnamedSemaphore *sem, unsigned int millisec, sys_call_error_fun fun)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(*sem, millisec);

    if (res == WAIT_FAILED)
    {
        sys_call_error("WaitForSingleObject");
        return 1;
    }
 
    if (res == WAIT_OBJECT_0) return 0;

    if (res == WAIT_TIMEOUT) return 2;

    return 1;
}
#else
{
    int res = 0;
    struct timeval tv;
    struct timespec timeout;
    
    if(gettimeofday(&tv, NULL) == -1)
    {
        sys_call_error("gettimeofday");
        return 1;
    }

    timeout.tv_sec = tv.tv_sec  + millisec / 1000;
    timeout.tv_nsec = tv.tv_usec * 1000;

    res = pthread_mutex_lock(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_lock");
        return 1;
    }

    while (sem->count <= 0)
    {
        //d_printf1("timedwait: cycle\n");
        res = pthread_cond_timedwait(&(sem->condition), &(sem->mutex), &timeout);
        if (res == ETIMEDOUT)
        {
            res = pthread_mutex_unlock(&(sem->mutex));
            if (res != 0)
            {
                sys_call_error("pthread_mutex_unlock");
                return 1;
            }
            //d_printf1("timedwait: timeout\n");
            return 2;
        }
        else if (res != 0)
        {
            sys_call_error("pthread_cond_timedwait");
            return 1;
        }
    }

    sem->count--;

    res = pthread_mutex_unlock(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_unlock");
        return 1;
    }

    //d_printf1("timedwait: signal\n");
    return 0;
} 
#endif


int UUnnamedSemaphoreUp(UUnnamedSemaphore *sem, sys_call_error_fun fun)
#ifdef _WIN32
{
    BOOL res;
    res = ReleaseSemaphore(*sem, 1, NULL);

    if (res == 0) 
    {
        sys_call_error("ReleaseSemaphore");
        return 1;
    }

    return 0;
}
#else
{
    int res = 0;

    res = pthread_mutex_lock(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_lock");
        return 1;
    }

    sem->count++;

    res = pthread_mutex_unlock(&(sem->mutex));
    if (res != 0)
    {
        sys_call_error("pthread_mutex_unlock");
        return 1;
    }

    res = pthread_cond_signal(&(sem->condition));
    if (res != 0)
    {
        sys_call_error("pthread_cond_signal");
        return 1;
    }

    return 0;
}
#endif


