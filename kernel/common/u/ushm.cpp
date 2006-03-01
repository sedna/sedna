/*
 * File:  ushm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "ushm.h"
#include "d_printf.h"

#define RIGHTS		0666

int uCreateShMem(UShMem *id, global_name name, int size)
#ifdef _WIN32
{
    //d_printf2("uCreateShMem name = %s\n", name);
    *id = CreateFileMapping(INVALID_HANDLE_VALUE,
                            NULL,
                            PAGE_READWRITE,
                            0,
                            size,
                            name
                           );

    //d_printf2("CreateFileMapping %s\n", name);
 
    if (*id == NULL) 
    {
        d_perror("CreateFileMapping");
        return 1;
    }

    return 0;
}
#else
{
	if(name == IPC_PRIVATE)
	{
		d_printf2("Key value %x is used for special cases\n", (int)name);
		return 1;
	}

	*id = shmget(name, size, IPC_CREAT | IPC_EXCL | RIGHTS);

	if(*id == -1)
	{
        d_perror("shmget");
		//d_printf2("Error %d\n", perror(semget));
		return 1;
	}
/* 
	shmid_ds info;

	info.shm_perm.uid = geteuid();
	info.shm_perm.gid = getegid();
	info.shm_perm.mode |= 0600;

	if (shmctl(*id, IPC_SET, &info) == -1)
	{
        d_perror("shmctl");
		return 1;
	}
*/
	return 0;
}
#endif

int uOpenShMem(UShMem *id, global_name name, int size)
#ifdef _WIN32
{
    //d_printf2("uOpenShMem name = %s\n", name);
    *id = OpenFileMapping(FILE_MAP_ALL_ACCESS,				// Read/write permission. 
                          FALSE,							// Do not inherit the name
                          name								// of the mapping object. 
                         );

    //d_printf2("OpenFileMapping %s\n", name);
 
    if (*id == NULL) 
    {
        d_perror("OpenFileMapping");
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

    //d_printf2("name = %d\n", name);
	*(id) = shmget(name, size, 0);

	if(*id == -1)
	{
        d_perror("shmget");
		//d_printf2("Error %d\n", perror(semget));
		return 1;
	}

	return 0;
}
#endif

int uReleaseShMem(UShMem id)
#ifdef _WIN32
{
    BOOL res = 0;
    res = CloseHandle(id);
    if (res == 0) 
	{
        d_perror("CloseHandle");
		return 1;
	}

    return 0;
}
#else
{
	if(id < 0)
	{
		fprintf(stderr, "uReleaseShMem failed\n");
		//d_printf2("Error %d\n", perror(semget));
		return 1;
	}
	else
	{
		if (shmctl(id, IPC_RMID, NULL) < 0)
		{
			// if shared memory already destroyed don't raise an error
			if (errno == EINVAL) return 0;
            d_perror("shmctl");
			return 1;
		}
	}
	return 0;
}
#endif

int uCloseShMem(UShMem id)
#ifdef _WIN32
{
    BOOL res = 0;
    res = CloseHandle(id);
    if (res == 0) 
	{
        d_perror("CloseHandle");
		return 1;
	}

    return 0;
}
#else
{
	return 0;
}
#endif

void* uAttachShMem(UShMem id, void *ptr, int size)
#ifdef _WIN32
{
    void *res = NULL;
    res = MapViewOfFileEx(id,						// Handle to mapping object. 
                          FILE_MAP_ALL_ACCESS,		// Read/write permission. 
                          0,
                          0,
                          size,
                          ptr
                         );

    if (res == NULL) 
    {
        d_perror("MapViewOfFileEx");
        return NULL;
    }

    return res;
}
#else
{
	void *res = NULL;
	if ((int)(res = shmat(id, ptr, 0)) == -1)
	{
        d_perror("shmat");
    	return NULL;
	}
	return res;
}
#endif

int uDettachShMem(UShMem id, void * ptr)
#ifdef _WIN32
{
     BOOL res = 0;
     res = UnmapViewOfFile(ptr);
    if (res == 0) 
	{
        d_perror("UnmapViewOfFile");
		return 1;
	}

    return 0;
}
#else
{
	if(shmdt(ptr) < 0)
	{
        d_perror("shmdt");
		//d_printf2("Error %d\n", perror(semget));
		return 1;
	}
	return 0;
}
#endif


