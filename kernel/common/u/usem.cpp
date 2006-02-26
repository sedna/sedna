/*
 * File:  usem.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "usem.h"
#include "d_printf.h"
#include "utils.h"
#include <string>

using namespace std;

#define RIGHTS		0x1FF


///////////////////////////////////////////////////////////////////////////////
// Semaphore implementation
///////////////////////////////////////////////////////////////////////////////

int USemaphoreCreate(USemaphore *sem, int init_value, int max_value, global_name name)
#ifdef _WIN32
{
    *sem = CreateSemaphore(NULL, init_value, max_value, name);

    if (*sem == NULL)
    {
        d_printf1("CreateSemaphore failed\n");
        d_printf2("Error %d\n", GetLastError());
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

    return 0;
}
#else
{
	union {
	    int val;
	    struct semid_sd *buf;
	    ushort *array;
	} semctl_arg;

	if (name == IPC_PRIVATE)
	{
		d_printf2("Key value %x is used for special cases\n", (int)name);
		return 1;
	}

    int res = semget(name, SEM_AMOUNT, IPC_CREAT | IPC_EXCL | RIGHTS);
    *sem = res;

    if (*sem < 0)
    {
        d_perror("semget");
        return 1;
    }
	semctl_arg.val = init_value;

	if(semctl(*sem, 0, SETVAL, semctl_arg) < 0)
	{
        d_perror("semctl");
		return 1;
	}

    return 0;
}
#endif


int USemaphoreOpen(USemaphore *sem, global_name name)
#ifdef _WIN32
{
    *sem = OpenSemaphore(SEMAPHORE_ALL_ACCESS, FALSE, name);

    if (*sem == NULL)
    {
        //d_printf1("OpenSemaphore failed\n");
        //d_printf2("Error %d\n", GetLastError());
        return 1;
    }

    return 0;
}
#else
{
	if (name == IPC_PRIVATE)
	{
		d_printf2("Key value %x is used for special cases\n", (int)name);	
		return 1;
	}

    *sem = semget(name, 1, 0);

    if (*sem < 0)
    {
        d_perror("semget");
        return 1;
    }

    return 0;
}
#endif

int USemaphoreRelease(USemaphore sem)
#ifdef _WIN32
{
    BOOL res =  CloseHandle(sem);

    if (res == 0)
    {
        d_printf1("CloseHandle failed\n");
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }

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
            d_perror("semctl");
			return 1;
		}
	}
    return 0;
}
#endif


int USemaphoreClose(USemaphore sem)
#ifdef _WIN32
{
    BOOL res =  CloseHandle(sem);

    if (res == 0)
    {
        d_printf1("CloseHandle failed\n");
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }

    return 0;
}
#else
{
    return 0;
}
#endif

int USemaphoreDown(USemaphore sem)
#ifdef _WIN32
{
    DWORD res; 
    res = WaitForSingleObject(sem, INFINITE);
    if (res == WAIT_FAILED || res != WAIT_OBJECT_0)
    {
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }

    return 0;
}
#else
{
    int res;
	
    static struct sembuf op_op[1] = {{0, -1, 0}};

    while (true)
    {
        res = semop(sem, &op_op[0], 1);
	
        if (res == 0) return 0;
        else if (errno == EINTR) continue;
        else 
        {
            d_perror("semop");
            return 1;
        }
    }
}
#endif


int USemaphoreDown(USemaphore sem, unsigned int millisec)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(sem, millisec);

    if (res == WAIT_FAILED)
    {
        d_printf2("Error %d\n", GetLastError());
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
    
    static struct sembuf op_op[1] = {{0, -1, IPC_NOWAIT}};
        
    for(; count < (millisec/1000); count++)
    {    
    	res = semop(sem, &op_op[0], 1);
	
    	if (res == 0) return 0; 
        else if (errno == EAGAIN)
        {
            sleep(1);
        }
        else 
        {
            d_perror("semop");
            return 1;
        }
    }

    return 2;
} 
#endif

int USemaphoreUp(USemaphore sem)
#ifdef _WIN32
{
    BOOL res;

    res = ReleaseSemaphore(sem, 1, NULL);

    if (res == 0) 
    {
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }

    return 0;
}
#else
{
	int res;
	
	static struct sembuf op_op[1] = {{0, 1, IPC_NOWAIT}};

	res = semop(sem, &op_op[0], 1);	
	if(res < 0)
	{
        d_perror("semop");
		return 1;	
	}
	return 0;
}
#endif






///////////////////////////////////////////////////////////////////////////////
// Array of semaphore implementation
///////////////////////////////////////////////////////////////////////////////

int USemaphoreArrCreate(USemaphoreArr *sem, int size, const int *init_values, global_name name)
#ifdef _WIN32
{
    int i = 0;
    *sem = new HANDLE[size];
    string base_name(name);

    for (i = 0; i < size; i++)
    {
        (*sem)[i] = CreateSemaphore(NULL,
                                    init_values[i],
                                    INT_MAX,
                                    (base_name + int2string(i)).c_str());

        if ((*sem)[i] == NULL)
        {
            d_printf1("CreateSemaphore failed\n");
            d_printf2("Error %d\n", GetLastError());
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
    }

    return 0;
}
#else
{
	union {
	    int val;
	    struct semid_sd *buf;
	    ushort *array;
	} semctl_arg;

	if (name == IPC_PRIVATE)
	{
		d_printf2("Key value %x is used for special cases\n", (int)name);
		return 1;
	}

    *sem = semget(name, size, IPC_CREAT | IPC_EXCL | RIGHTS);

    if (*sem < 0)
    {
        d_printf1("USemaphoreArrCreate failed\n");
        //d_printf2("Error %d\n", perror(semget));
        return 1;
    }

    for (int i = 0; i < size; i++)
    {
	    semctl_arg.val = init_values[i];

        if(semctl(*sem, i, SETVAL, semctl_arg) < 0)
        {
		    //d_printf3("ERRROR: %d. Can't set initial value for %x semaphore\n", perror(semctl), (int)name);
		    d_printf2("Can't set initial value for %x semaphore\n", (int)name);
		     return 1;
    	}
    }

    return 0;
}
#endif

int USemaphoreArrOpen(USemaphoreArr *sem, int size, global_name name)
#ifdef _WIN32
{
    int i = 0;
    *sem = new HANDLE[size];
    string base_name(name);

    for (i = 0; i < size; i++)
    {
        (*sem)[i] = OpenSemaphore(SEMAPHORE_ALL_ACCESS, 
                                  FALSE, 
                                  (base_name + int2string(i)).c_str());

        if ((*sem)[i] == NULL)
        {
            d_printf1("OpenSemaphore failed\n");
            d_printf2("Error %d\n", GetLastError());
            return 1;
        }
    }

    return 0;
}
#else
{
	if (name == IPC_PRIVATE)
	{
		d_printf2("Key value %x is used for special cases\n", (int)name);	
		return 1;
	}

    *sem = semget(name, size, 0);

    if (*sem < 0)
    {
        d_printf1("USemaphoreArrOpen failed\n");
        //d_printf2("Error %d\n", perror(semget));
        return 1;
    }

    return 0;
}
#endif


int USemaphoreArrRelease(USemaphoreArr sem, int size)
#ifdef _WIN32
{
    BOOL res;

    for (int i = 0; i < size; i++)
    {
        res = CloseHandle(sem[i]);

        if (res == 0)
        {
            d_printf1("CloseHandle failed\n");
            d_printf2("Error %d\n", GetLastError());
            return 1;
        }
    }

    delete [] sem;

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
			d_printf1("Can't do IPC_RMID\n");
			return 1;
		}
	}
    return 0;
}
#endif


int USemaphoreArrClose(USemaphoreArr sem, int size)
#ifdef _WIN32
{
    BOOL res;

    for (int i = 0; i < size; i++)
    {
        res = CloseHandle(sem[i]);

        if (res == 0)
        {
            d_printf1("CloseHandle failed\n");
            d_printf2("Error %d\n", GetLastError());
            return 1;
        }
    }

    delete [] sem;

    return 0;
}
#else
{
    return 0;
}
#endif

int USemaphoreArrDown(USemaphoreArr sem, int i)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(sem[i], INFINITE);

    if (res == WAIT_FAILED || res != WAIT_OBJECT_0)
    {
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }

    return 0;
}
#else
{
    int res;
	
    struct sembuf op_op[1] = {{0, -1, 0}};
    op_op[0].sem_num = i;

    while (true)
    {
        res = semop(sem, &op_op[0], 1);
	
        if (res == 0) return 0;
        else if (errno == EINTR) continue;
        else 
        {
            perror("semop");
            d_printf2("USemaphoreArrDown error (index - %d)\n", i);
            return 1;
        }
    }
}
#endif


int USemaphoreArrDown(USemaphoreArr sem, int i, unsigned int millisec)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(sem[i], millisec);

    if (res == WAIT_FAILED)
    {
        d_printf2("Error %d\n", GetLastError());
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
    
    static struct sembuf op_op[1] = {{0, -1, IPC_NOWAIT}};
    op_op[0].sem_num = i;
        
    for(; count < (millisec/1000); count++)
    {    
    	res = semop(sem, &op_op[0], 1);
	
    	if(res == 0) return 0; 
    	//else sleep(1);
    }

    return 2;
} 
#endif

int USemaphoreArrUp(USemaphoreArr sem, int i)
#ifdef _WIN32
{
    BOOL res;

    res = ReleaseSemaphore(sem[i], 1, NULL);

    if (res == 0) 
    {
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }

    return 0;
}
#else
{
	int res;
	
	static struct sembuf op_op[1] = {{0, 1, IPC_NOWAIT}};
    op_op[0].sem_num = i;

	res = semop(sem, &op_op[0], 1);
	
	if(res < 0)
	{
		d_printf1("Error decremetning\n");
		//d_printf2("Error decremetning %x\n", perror(semop));
		return 1;	
	}
	return 0;
}
#endif




///////////////////////////////////////////////////////////////////////////////
// Unnamed semaphore implementation
///////////////////////////////////////////////////////////////////////////////

int UUnnamedSemaphoreCreate(UUnnamedSemaphore *sem, int init_value)
#ifdef _WIN32
{
    *sem = CreateSemaphore(NULL, init_value, INT_MAX, NULL);

    if (*sem == NULL)
    {
        d_printf1("CreateSemaphore failed\n");
        d_printf2("Error %d\n", GetLastError());
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
        d_printf1("pthread_mutex_init failed\n");
        d_perror("pthread_mutex_init");
        return 1;
    }

    res = pthread_cond_init(&(sem->condition), NULL);
    if (res != 0)
    {
        d_printf1("pthread_cond_init failed\n");
        d_perror("pthread_cond_init");
        pthread_mutex_destroy(&(sem->mutex));
        return 1;
    }

    sem->count = init_value;

    return 0;
}
#endif

int UUnnamedSemaphoreRelease(UUnnamedSemaphore *sem)
#ifdef _WIN32
{
    BOOL res =  CloseHandle(*sem);

    if (res == 0)
    {
        d_printf1("CloseHandle failed\n");
        d_printf2("Error %d\n", GetLastError());
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
        d_printf1("pthread_mutex_destroy failed\n");
        d_perror("pthread_mutex_destroy");
        return 1;
    }

    res = pthread_cond_destroy(&(sem->condition));
    if (res != 0)
    {
        d_printf1("pthread_cond_destroy failed\n");
        d_perror("pthread_cond_destroy");
        return 1;
    }

    return 0;
}
#endif

int UUnnamedSemaphoreDown(UUnnamedSemaphore *sem)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(*sem, INFINITE);

    if (res == WAIT_FAILED || res != WAIT_OBJECT_0)
    {
        d_printf2("Error %d\n", GetLastError());
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
        d_printf1("pthread_mutex_lock failed\n");
        d_perror("pthread_mutex_lock");
        return 1;
    }

    while (sem->count <= 0)
    {
        res = pthread_cond_wait(&(sem->condition), &(sem->mutex));
        if (res != 0)
        {
            d_printf1("pthread_cond_wait failed\n");
            d_perror("pthread_cond_wait");
            return 1;
        }
    }

    sem->count--;

    res = pthread_mutex_unlock(&(sem->mutex));
    if (res != 0)
    {
        d_printf1("pthread_mutex_unlock failed\n");
        d_perror("pthread_mutex_unlock");
        return 1;
    }

    return 0;
}
#endif

// down with timout
// return values: 0 - success
//                1 - falure
//                2 - timeout
int UUnnamedSemaphoreDown(UUnnamedSemaphore *sem, unsigned int millisec)
#ifdef _WIN32
{
    DWORD res;
    res = WaitForSingleObject(*sem, millisec);

    if (res == WAIT_FAILED)
    {
        d_printf2("Error %d\n", GetLastError());
        return 1;
    }
 
    if (res == WAIT_OBJECT_0) return 0;

    if (res == WAIT_TIMEOUT) return 2;

    return 1;
}
#else
{
    int res = 0;

    timespec timeout;
    timeout.tv_sec = time(NULL) + millisec / 1000;
    timeout.tv_nsec = 0;

    res = pthread_mutex_lock(&(sem->mutex));
    if (res != 0)
    {
        d_printf1("pthread_mutex_lock failed\n");
        d_perror("pthread_mutex_lock");
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
                d_printf1("pthread_mutex_unlock failed\n");
                d_perror("pthread_mutex_unlock");
                return 1;
            }
            //d_printf1("timedwait: timeout\n");
            return 2;
        }
        else if (res != 0)
        {
            d_printf1("pthread_cond_wait failed\n");
            d_perror("pthread_cond_wait");
            return 1;
        }
    }

    sem->count--;

    res = pthread_mutex_unlock(&(sem->mutex));
    if (res != 0)
    {
        d_printf1("pthread_mutex_unlock failed\n");
        d_perror("pthread_mutex_unlock");
        return 1;
    }

    //d_printf1("timedwait: signal\n");
    return 0;
} 
#endif


int UUnnamedSemaphoreUp(UUnnamedSemaphore *sem)
#ifdef _WIN32
{
    BOOL res;
    res = ReleaseSemaphore(*sem, 1, NULL);

    if (res == 0) 
    {
        d_printf2("Error %d\n", GetLastError());
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
        d_printf1("pthread_mutex_lock failed\n");
        d_perror("pthread_mutex_lock");
        return 1;
    }

    sem->count++;

    res = pthread_mutex_unlock(&(sem->mutex));
    if (res != 0)
    {
        d_printf1("pthread_mutex_unlock failed\n");
        d_perror("pthread_mutex_unlock");
        return 1;
    }

    res = pthread_cond_signal(&(sem->condition));
    if (res != 0)
    {
        d_printf1("pthread_cond_signal failed\n");
        d_perror("pthread_cond_signal");
        return 1;
    }

    return 0;
}
#endif


