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

#ifndef _WIN32
union semctl_arg_t {
    int val;
    struct semid_sd *buf;
    ushort *array;
};
#endif /* _WIN32 */

int USemaphoreCreate(USemaphore *sem, int init_value, int max_value, global_name gname, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
    struct gobj_info_t info = {GOBJECT_SEM, sem};
#ifdef _WIN32
    GLOBAL_NAME_BUFFER_DECL(objectName);
    UGetNameFromGlobalName(gname, objectName, sizeof objectName);

    *sem = CreateSemaphore(sa, init_value, max_value, objectName);

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
#else /* _WIN32 */
    union semctl_arg_t semctl_arg;
    key_t key = USys5IPCKeyFromGlobalName(gname);

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCleanup(gname, info); };

    USECURITY_ATTRIBUTES sem_access_mode = U_SEDNA_SEMAPHORE_ACCESS_PERMISSIONS_MASK;

    if (sa) {
        sem_access_mode = *sa;
    }

    int result = semget(key, 1, IPC_CREAT | IPC_EXCL | sem_access_mode);

    if (result < 0)
    {
        sys_call_error("semget");
        return 1;
    }

    *sem = result;

    semctl_arg.val = init_value;

    if (semctl(*sem, 0, SETVAL, semctl_arg) < 0)
    {
        sys_call_error("semctl");
        return 1;
    }
#endif /* _WIN32 */

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(gname, info); };

    return 0;
}


int USemaphoreOpen(USemaphore *sem, global_name gname, sys_call_error_fun fun)
{
#ifdef _WIN32
    GLOBAL_NAME_BUFFER_DECL(objectName);
    UGetNameFromGlobalName(gname, objectName, sizeof objectName);

    *sem = OpenSemaphore(SEMAPHORE_ALL_ACCESS, FALSE, objectName);

    if (*sem == NULL)
    {
        sys_call_error("OpenSemaphore");
        return 1;
    }

#else /* _WIN32 */
    key_t key = USys5IPCKeyFromGlobalName(gname);

    *sem = semget(key, 1, 0);

    if (*sem < 0)
    {
        sys_call_error("semget");
        return 1;
    }

#endif /* _WIN32 */

    return 0;
}

int USemaphoreRelease(USemaphore sem, sys_call_error_fun fun)
{
    struct gobj_info_t info = {GOBJECT_SEM, &sem};
#ifdef _WIN32
    BOOL res =  CloseHandle(sem);

    if (res == 0)
    {
        sys_call_error("CloseHandle");
        return 1;
    }
#else
    if (sem < 0) {
        return 1;
    }

    if (semctl(sem, 0, IPC_RMID) < 0) {
        sys_call_error("semctl");
        return 1;
    }
#endif

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(GN_NULL, info); };

    return 0;
}


int USemaphoreClose(USemaphore sem, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = CloseHandle(sem);

    if (res == 0)
    {
        sys_call_error("CloseHandle");
        return 1;
    }
#endif

    return 0;
}

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

int USemaphoreArrCreate(USemaphoreArr *sem, unsigned size, const int *init_values, global_name gname, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
    struct gobj_info_t info = {GOBJECT_SEM_ARRAY, sem, size};
#ifdef _WIN32
    GLOBAL_NAME_BUFFER_DECL(objectName);
    unsigned i = 0;
	size_t name_len = 0;

    if (UGetNameFromGlobalName(gname, objectName, sizeof objectName) != NULL) 
	{
		name_len = strlen(objectName);
	}

    *sem = (USemaphoreArr) malloc(sizeof(HANDLE) * size);

    for (i = 0; i < size; i++)
    {
        sprintf(objectName + name_len,":%u", i);

        (*sem)[i] = CreateSemaphore(sa,
                                    init_values[i],
                                    INT_MAX,
                                    objectName);

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
#else
    union semctl_arg_t semctl_arg;
    key_t key = USys5IPCKeyFromGlobalName(gname);
    unsigned i = 0;

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCleanup(gname, info); };

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
            sys_call_error("semctl");
            return 1;
        }
    }
#endif

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onCreate(gname, info); };

    return 0;
}

int USemaphoreArrOpen(USemaphoreArr *sem, unsigned size, global_name gname, sys_call_error_fun fun)
#ifdef _WIN32
{
    GLOBAL_NAME_BUFFER_DECL(objectName);
	size_t name_len = 0;
	unsigned i = 0;

    if (NULL != UGetNameFromGlobalName(gname, objectName, sizeof objectName))
	{
		name_len = strlen(objectName);
	}
    

    *sem = (USemaphoreArr)malloc(sizeof(HANDLE) * size);

    for (i = 0; i < size; i++)
    {
		sprintf(objectName+name_len,":%u",i);

        (*sem)[i] = OpenSemaphore(SEMAPHORE_ALL_ACCESS, 
                                  FALSE, 
                                  objectName);

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
    key_t key = USys5IPCKeyFromGlobalName(gname);

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
{
    struct gobj_info_t info = {GOBJECT_SEM_ARRAY, &sem};
#ifdef _WIN32
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
#else
    if (sem < 0) {
        return 1;
    } else {
        if (semctl(sem, 0, IPC_RMID/*, semctl_arg*/) < 0)
        {
            sys_call_error("semctl");
            return 1;
        }
    }

#endif

    if (UGlobalObjectsGC) { UGlobalObjectsGC->onDestroy(GN_NULL, info); };
    return 0;
}


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


