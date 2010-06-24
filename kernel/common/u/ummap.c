/*
 * File:  ummap.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/u/ummap.h"
#include "common/errdbg/d_printf.h"
#include "common/u/ugnames.h"

UMMap uCreateFileMapping(UFile fd, size_t size, const char* name, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
	char buf[128];
	const char *uName = NULL;
    UMMap m;
#ifdef _WIN32
    DWORD size_high, size_low;

#ifdef _WIN64
    size_high = size >> 32;
#else
    size_high = 0;
#endif
    size_low = (DWORD)size;

    m.fd = fd;
	uName = UWinIPCNameFromGlobalName(name, buf, sizeof buf);
    m.map = CreateFileMapping(fd, sa, PAGE_READWRITE, size_high, size_low, uName);
    if (m.map == NULL) sys_call_error("CreateFileMapping");

    if (m.map == NULL || GetLastError() == ERROR_ALREADY_EXISTS) m.map = NULL;

    return m;
#else
	uName = UPosixIPCNameFromGlobalName(name, buf, sizeof buf);
    if (fd == U_INVALID_FD)
    {
        USECURITY_ATTRIBUTES mmap_access_mode = U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK;
        if (sa) mmap_access_mode = *sa;
        m.map = shm_open(uName, O_RDWR | O_CREAT | O_EXCL, mmap_access_mode);
        m.size = size;
        m.to_file = 0;
        if (m.map == -1)
        {
            sys_call_error("shm_open");
            return m;
        }

        if (ftruncate(m.map, (off_t)size) == -1)
        {
            sys_call_error("ftruncate");
            m.map = -1;
            return m;
        }
    }
    else
    {
        struct stat buf;
        if (fstat(fd, &buf) == -1)
        {
            sys_call_error("fstat");
            m.map = -1;
            return m;
        }

        m.map = fd;
        m.size = buf.st_size;
        m.to_file = 1;
    }

    return m;
#endif
}

UMMap uOpenFileMapping(UFile fd, size_t size, const char *name, sys_call_error_fun fun)
{
	char buf[128];
	const char *uName = NULL;
#ifdef _WIN32
    UMMap m;
    m.fd = INVALID_HANDLE_VALUE;
	uName = UWinIPCNameFromGlobalName(name, buf, sizeof buf);
    m.map = OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, uName);
    if (m.map == NULL) sys_call_error("OpenFileMapping");
    return m;
#else
    UMMap m;
	uName = UPosixIPCNameFromGlobalName(name, buf, sizeof buf);
    if (fd == U_INVALID_FD)
    {
        m.map = shm_open(uName, O_RDWR, 0);
        m.size = size;
        m.to_file = 0;
        if (m.map == -1)
        {
            sys_call_error("shm_open");
            return m;
        }
    }
    else
    {
        struct stat buf;
        if (fstat(fd, &buf) == -1)
        {
            sys_call_error("fstat");
            m.map = -1;
            return m;
        }

        m.map = fd;
        m.size = (size_t)buf.st_size;
        m.to_file = 1;
    }

    return m;
#endif
}

int uReleaseFileMapping(UMMap m, const char *name, sys_call_error_fun fun)
{
	char buf[128];
	const char *uName = NULL;
#ifdef _WIN32
    if( CloseHandle(m.map) == 0)
    {
        sys_call_error("CloseHandle");
        return -1;
    }
    else return 0;
#else
	uName = UPosixIPCNameFromGlobalName(name, buf, sizeof buf);
    if (uName)
    {
        if(0 == m.to_file && -1 != m.map)
        {
            if(close(m.map) == -1)
            {
                sys_call_error("close");
                return -1;            
            }
        }
        
        if (shm_unlink(uName) == -1)
        {
            sys_call_error("shm_unlink");
            return -1;
        }
    }

    return 0;
#endif
}

int uCloseFileMapping(UMMap m, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (CloseHandle(m.map) == 0)
    {
       sys_call_error("CloseHandle");
       return -1;
    }
    else return 0;
#else
    if (0 == m.to_file && -1 != m.map)
    {
        if (close(m.map) == -1)
        {
            sys_call_error("close");
            return -1;
        }
    }

    return 0;
#endif
}

void *uMapViewOfFile(UMMap m, void *addr, size_t size, uint64_t offs, sys_call_error_fun fun)
{
#ifdef _WIN32
    void *ret_val;
    DWORD off_high, off_low;

    off_high = offs >> 32;
    off_low = (DWORD)offs;

    if ((ret_val = MapViewOfFileEx(m.map, FILE_MAP_ALL_ACCESS, off_high, off_low, size, addr)) == NULL)
    {
       sys_call_error("MapViewOfFileEx");
       return ret_val;
    }
    else return ret_val;
#else
    if (size == 0) size = m.size;
    void* ret_val;

    if (addr)
      ret_val = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, m.map, (off_t)offs);
    else
      ret_val = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_SHARED, m.map, (off_t)offs);

    if (ret_val == MAP_FAILED)
        sys_call_error("mmap");

    return ret_val == MAP_FAILED ? NULL : ret_val;
    
#endif
}

int uUnmapViewOfFile(UMMap m, void *addr, size_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (UnmapViewOfFile(addr) == 0)
    {
       sys_call_error("UnmapViewOfFile");
       return -1;
    }
    else return 0;
#else
    if (size == 0) size = m.size;

    int res;
    if ((res = munmap(addr, size)) == -1)
    {
       sys_call_error("munmap");
       return res;
    }
    else return res;
#endif
}

int uFlushViewOfFile(UMMap m, void *addr, size_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (FlushViewOfFile(addr, size) == 0)
    {
       sys_call_error("FlushViewOfFile");
       return -1;
    }
    else return 0;
#else
    if (m.to_file)
    {
        int res;
        if (size == 0) size = m.size;
        if ((res = msync(addr, size, MS_SYNC)) == -1)
        {
          sys_call_error("msync");
          return res;
        }
        else return res;
    }
    else return 0;
#endif
}

int uMemLock(void *addr, size_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (VirtualLock(addr, size) == 0)
    {
       sys_call_error("VirtualLock");
       return -1;
    }
    else return 0;
#else
    int res = mlock(addr, size);
    if (res == -1) sys_call_error("mlock");
    return res;
#endif
}

int uMemUnlock(void *addr, size_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    if (VirtualUnlock(addr, size) == 0)
    {
       sys_call_error("VirtualUnlock");
       return -1;
    }
    else return 0;
#else
    int res = munlock(addr, size);
    if (res == -1) 
       sys_call_error("munlock");
    return res;
#endif
}
