/*
 * File:  usafesync.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _USAFESYNC_H
#define _USAFESYNC_H

#include "u/u.h"
#include "u/usem.h"
#include "u/uthread.h"

/*
 * Exception safe syncronisation
 */

struct SafeSemaphore {
private:
    const USemaphore _sem;
    bool _aquired;
public:
    inline void Aquire() { USemaphoreDown(_sem, __sys_call_error); _aquired = true; };
    inline void Release() { USemaphoreUp(_sem, __sys_call_error); _aquired = false; };
    inline ~SafeSemaphore() { if (_aquired) { USemaphoreUp(_sem, __sys_call_error); } };
    inline SafeSemaphore(const USemaphore &sem) : _sem(sem), _aquired(false) { };
};

/*
struct SafeSpinlock {
private:
    const USpinLockPointer _slock;
    bool _aquired;
public:
    inline void Aquire() { uSpinLock(_slock); _aquired = true; };
    inline void Release() { uSpinUnlock(_slock); _aquired = false; };
    inline ~SafeSpinlock() { if (_aquired) { uSpinUnlock(_slock); } };
    inline SafeSpinlock(const USpinLockPointer slock) : _slock(slock), _aquired(false) { };
};
*/

#endif
