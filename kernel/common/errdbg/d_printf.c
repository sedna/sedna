/*
 * File:  d_printf.c
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/errdbg/d_printf.h"


int el_debug_printf(const char *s, ...)
{
#ifdef EL_DEBUG_SYNC
    va_list ap;
    int res = 0;

    el_debug_sync_lock();

    va_start(ap, s);
    if (SEDNA_EL_DEBUG_DUPLICATE_TO_STDERR)
    {
        res = vfprintf(stderr, s, ap);
        if (res < 0)
            goto el_debug_printf_exit;
    }

    if (el_debug_sync_output_stream)
    {
        res = fseek(el_debug_sync_output_stream, 0, SEEK_END);
        if (res < 0)
            goto el_debug_printf_exit;

        res = vfprintf(el_debug_sync_output_stream, s, ap);
    }

el_debug_printf_exit:
    va_end(ap);
    el_debug_sync_unlock();

    return res;
#else
    va_list ap;
    int res = 0;

    va_start(ap, s);
    res = vfprintf(stderr, s, ap);
    va_end(ap);

    return res;
#endif
}

void el_debug_perror(const char *s)
{
#ifdef EL_DEBUG_SYNC
    el_debug_sync_lock();

    if (SEDNA_EL_DEBUG_DUPLICATE_TO_STDERR)
        uperror(s);

    if (el_debug_sync_output_stream)
    {
        int res = fseek(el_debug_sync_output_stream, 0, SEEK_END);
        if (res < 0)
            goto el_debug_perror_exit;

        res = fprintf(stderr, "%s: %s\n", s, ustrerror(errno));
    }

el_debug_perror_exit:
    el_debug_sync_unlock();
#else
    uperror(s);
#endif
}

void el_debug_flush()
{
#ifdef EL_DEBUG_SYNC
    el_debug_sync_lock();

    if (SEDNA_EL_DEBUG_DUPLICATE_TO_STDERR)
    {
        fflush(stderr);
    }

    if (el_debug_sync_output_stream)
    {
        fflush(el_debug_sync_output_stream);
    }
    el_debug_sync_unlock();
#else
    /* to improve peformance we should leave the body empty */
#endif
}

#ifdef EL_DEBUG_SYNC

FILE *el_debug_sync_output_stream = NULL;
#ifdef _WIN32
HANDLE el_debug_sync_sem = (HANDLE) 0;
#endif

void el_debug_sync_release()
{
#ifdef _WIN32
    if (el_debug_sync_sem)
        CloseHandle(el_debug_sync_sem);

    el_debug_sync_sem = (HANDLE) 0;
#endif

    if (el_debug_sync_output_stream)
    {
        fclose(el_debug_sync_output_stream);
        el_debug_sync_output_stream = NULL;
    }
}

void el_debug_sync_lock()
{
    if (!el_debug_sync_output_stream)
    {
        el_debug_sync_output_stream = fopen(SEDNA_EL_DEBUG_FILE_NAME, "a");
    }

#ifdef _WIN32
    if (!el_debug_sync_sem)
    {
        el_debug_sync_sem = CreateSemaphore(NULL, 1, 1, SEDNA_EL_DEBUG_SEMAPHORE_NAME);
    }

    if (el_debug_sync_sem)
    {
        DWORD res;
        res = WaitForSingleObject(el_debug_sync_sem, INFINITE);
    }
#endif
}

void el_debug_sync_unlock()
{
#ifdef _WIN32
    if (el_debug_sync_sem)
    {
        BOOL res;
        res = ReleaseSemaphore(el_debug_sync_sem, 1, NULL);
    }
#endif
}

#endif
