/*
 * File:  ummap.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "ummap.h"
#include "d_printf.h"

//#define RIGHTS		00660

UMMap uCreateFileMapping(UFile fd, int size, const char* name, USECURITY_ATTRIBUTES* sa)
{
#ifdef _WIN32
    UMMap m;
    m.fd = fd;
    m.map = CreateFileMapping(fd, sa, PAGE_READWRITE, 0, size, name);
    if (m.map == NULL || GetLastError() == ERROR_ALREADY_EXISTS) m.map = NULL;

    return m;
#else
    UMMap m;
    if (fd == U_INVALID_FD)
    {
        USECURITY_ATTRIBUTES mmap_access_mode = U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK;
        if (sa) mmap_access_mode = *sa;
        m.map = shm_open(name, O_RDWR | O_CREAT | O_EXCL, mmap_access_mode);
        m.size = size;
        m.to_file = 0;
        if (m.map == -1)
        {
            d_perror("shm_open");
            return m;
        }

        if (ftruncate(m.map, size) == -1)
        {
            d_perror("ftruncate");
            m.map = -1;
            return m;
        }
    }
    else
    {
        struct stat buf;
        if (fstat(fd, &buf) == -1)
        {
            d_perror("fstat");
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

UMMap uOpenFileMapping(UFile fd, int size, const char *name)
{
#ifdef _WIN32
    UMMap m;
    m.fd = INVALID_HANDLE_VALUE;
    m.map = OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, name);
    return m;
#else
    UMMap m;
    if (fd == U_INVALID_FD)
    {
        m.map = shm_open(name, O_RDWR, 0);
        m.size = size;
        m.to_file = 0;
        if (m.map == -1)
        {
            d_perror("shm_open");
            return m;
        }
    }
    else
    {
        struct stat buf;
        if (fstat(fd, &buf) == -1)
        {
            d_perror("fstat");
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

int uReleaseFileMapping(UMMap m, const char *name)
{
#ifdef _WIN32
    return (CloseHandle(m.map) == 0 ? -1 : 0);
#else
    if (name)
    {
        if (shm_unlink(name) == -1)
        {
            perror("shm_unlink");
            return -1;
        }
    }

    return 0;
#endif
}

int uCloseFileMapping(UMMap m)
{
#ifdef _WIN32
    return (CloseHandle(m.map) == 0 ? -1 : 0);
#else
    return 0;
#endif
}

void *uMapViewOfFile(UMMap m, void *addr, int size, int offs)
{
#ifdef _WIN32
    return MapViewOfFileEx(m.map, FILE_MAP_ALL_ACCESS, 0, offs, size, addr);
#else
    if (size == 0) size = m.size;

    if (addr) return mmap(addr, size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, m.map, offs);
    else return mmap(addr, size, PROT_READ | PROT_WRITE, MAP_SHARED, m.map, offs);
#endif
}

int uUnmapViewOfFile(UMMap m, void *addr, int size)
{
#ifdef _WIN32
    return (UnmapViewOfFile(addr) == 0 ? -1 : 0);
#else
    if (size == 0) size = m.size;

    return munmap(addr, size);
#endif
}

int uFlushViewOfFile(UMMap m, void *addr, int size)
{
#ifdef _WIN32
    return (FlushViewOfFile(addr, size) == 0 ? -1 : 0);
#else
    if (m.to_file)
    {
        if (size == 0) size = m.size;
        return msync(addr, size, MS_SYNC);
    }
    else return 0;
#endif
}

int uMemLock(void *addr, size_t size)
{
#ifdef _WIN32
    return (VirtualLock(addr, size) == 0 ? -1 : 0);
#else
    int res = mlock(addr, size);
    if (res == -1) d_perror("mlock");
    return res;
#endif
}

int uMemUnlock(void *addr, size_t size)
{
#ifdef _WIN32
    return (VirtualUnlock(addr, size) == 0 ? -1 : 0);
#else
    int res = munlock(addr, size);
    if (res == -1) d_perror("munlock");
    return res;
#endif
}
