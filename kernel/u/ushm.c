/*
 * File:  ushm.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "u/ushm.h"
#include "u/ummap.h"
#include "common/errdbg/d_printf.h"
#include "u/ugnames.h"

#if (defined(SunOS))
/*
 * See https://bugzilla.mozilla.org/show_bug.cgi?id=263952
 * It's not easy to fix shm_open & shm_unlink warnigns unfortunately.
 * Anyway better to include this header.
 */
#include <sys/mman.h>
#endif

int uCreateShMem(UShMem *id, global_name name, size_t size, USECURITY_ATTRIBUTES* sa, sys_call_error_fun fun)
{
#ifdef _WIN32
    UMMap mmap = uCreateFileMapping(U_INVALID_FD, size, name, sa, fun);

    if (U_INVALID_FILEMAPPING(mmap))
        return 1;

    id->id = mmap.map;
    id->size = mmap.size;
#else
    char buf[128];
    const char *uName = NULL;
    uName = UPosixIPCNameFromGlobalName(name, buf, sizeof buf);

    USECURITY_ATTRIBUTES mmap_access_mode = U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK;
    if (sa) mmap_access_mode = *sa;

    id->id = shm_open(uName, O_RDWR | O_CREAT | O_EXCL, mmap_access_mode);
    id->size = size;
    id->to_guarantee =  1;

    if (id->id == -1)
    {
        sys_call_error("shm_open");
        return 1;
    }

    if (ftruncate(id->id, (off_t)size) == -1)
    {
        sys_call_error("ftruncate");
        close(id->id);
        id->id = -1;
        return 1;
    }
#endif

    return 0;
}

int uOpenShMem(UShMem *id, global_name name, sys_call_error_fun fun)
{
#ifdef _WIN32
    UMMap mmap = uOpenFileMapping(U_INVALID_FD, name, fun);

    if (U_INVALID_FILEMAPPING(mmap))
        return 1;

    id->id = mmap.map;
    id->size = mmap.size;
#else
    char buf[128];
    const char *uName = NULL;
    struct stat fbuf;

    uName = UPosixIPCNameFromGlobalName(name, buf, sizeof buf);

    if ((id->id = shm_open(uName, O_RDWR, 0)) == -1)
    {
        sys_call_error("shm_open");
        return 1;
    }

    if (fstat(id->id, &fbuf) == -1)
    {
        sys_call_error("fstat");
        close(id->id);
        return 1;
    }

    id->to_guarantee = 0;
    id->size = fbuf.st_size;

#endif

    return 0;
}

int uReleaseShMem(UShMem *id, global_name name, sys_call_error_fun fun)
{
#ifdef _WIN32
    UMMap mmap;

    mmap.map = id->id;
    mmap.size = id->size;

    return uReleaseFileMapping(mmap, name, fun);
#else
    char buf[128];
    const char *uName = NULL;
    uName = UPosixIPCNameFromGlobalName(name, buf, sizeof buf);

    if(id->id != -1 && close(id->id) == -1)
    {
        sys_call_error("close");
        return 1;
    }

    if (shm_unlink(uName) == -1)
    {
        sys_call_error("shm_unlink");
        return 1;
    }

    return 0;
#endif
}

int uCloseShMem(UShMem *id, sys_call_error_fun fun)
{
#ifdef _WIN32
    UMMap mmap;

    mmap.map = id->id;
    mmap.size = id->size;

    return uCloseFileMapping(mmap, fun);
#else
    if (id->id != -1 && close(id->id) == -1)
    {
        sys_call_error("close");
        return 1;
    }

    return 0;
#endif
}

void* uAttachShMem(UShMem *id, void *addr, size_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    UMMap mmap;

    mmap.map = id->id;
    mmap.size = id->size;

    return uMapViewOfFile(mmap, addr, size, 0, fun);
#else
    void* ret_val;
    int res;

    if (size == 0) size = id->size;

    if (addr)
      ret_val = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, id->id, 0);
    else
      ret_val = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_SHARED, id->id, 0);

    if (ret_val == MAP_FAILED)
        sys_call_error("mmap");

    /* now we can close fd (without error checking, its useless here) */

    close(id->id);
    id->id = -1;

    if (id->to_guarantee)
    {
        res = uGuaranteeSharedMemory(ret_val, size, fun);

        if (res != 0)
            ret_val = MAP_FAILED;
    }

    return ret_val == MAP_FAILED ? NULL : ret_val;
#endif
}

int uDettachShMem(UShMem *id, void *ptr, sys_call_error_fun fun)
{
#ifdef _WIN32
    UMMap mmap;

    mmap.map = id->id;
    mmap.size = id->size;
    return uUnmapViewOfFile(mmap, ptr, id->size, fun);
#else
    if (munmap(ptr, id->size) == -1)
    {
       sys_call_error("munmap");
       return 1;
    }

    return 0;
#endif
}

/*
 * Why do we need SIGBUS hack?
 *
 * Since Linux and possibly other OSs do shared memory overcommitment,
 * shm_open + ftruncate + mmap chain alone doesn't guarantee shared memory
 * segment of the requested size. In case we don't have enough memory, for
 * example, we'll receive SIGBUS on memory access. Here, we try to mark dirty
 * every page in the buffer pool enforcing OS to allocate the entire segment.
 * In this case we won't get SIGBUS on memory access.
 *
 * Another possible solution is to write() to shared-memory file before mmap,
 * but that would be much less effective on large segments, since we'd
 * essentially be enforced to write on every memory page belonging to the segment.
 */
#ifndef _WIN32

static sigjmp_buf jmpbuf;
static volatile sig_atomic_t canjump;

void _gbp_sigbus_handler(int signo)
{
    if (canjump == 0) return;
    canjump = 0;
    siglongjmp(jmpbuf, 1);
}

static int _guarantee_buffer_pool(void* addr, size_t size, sys_call_error_fun fun)
{
    int page_size = getpagesize();
    size_t total_pages = size / page_size, i;
    unsigned char* buf_mem = (unsigned char*) addr;
    struct sigaction sigbus_act, sig_backup;

    if (size % page_size != 0) total_pages++;

    memset(&sigbus_act, '\0', sizeof(struct sigaction));
    memset(&sig_backup, '\0', sizeof(struct sigaction));
    sigbus_act.sa_handler = _gbp_sigbus_handler;

    if (sigaction(SIGBUS, &sigbus_act, &sig_backup) == -1)
    {
        u_call_error("Can't set SIGBUS handler to preallocate buffer pool.");
        return -2;
    }

    if (sigsetjmp(jmpbuf, 1) != 0)
    {
#if defined(LINUX)
        u_call_error("Cannot preallocate shared memory. There are not enough system resources. Try to remount /dev/shm with a greater size.");
#else
        u_call_error("Cannot preallocate shared memory. There are not enough system resources.");
#endif
        return -1;
    }
    else
    {
        canjump = 1;
        for (i = 0; i < total_pages; i++)
            memset(buf_mem + i * page_size, '\0', sizeof(unsigned char));
    }

    if (sigaction(SIGBUS, &sig_backup, NULL) == -1)
    {
        u_call_error("Can't restore SIGBUS handler in buffer pool preallocation.");
        return -2;
    }

    return 0;
}

#endif

int uGuaranteeSharedMemory(void *addr, size_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    /* Nothing to do on Windows */
    return 0;
#else
    return _guarantee_buffer_pool(addr, size, fun);
#endif
}
