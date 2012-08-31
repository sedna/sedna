#ifndef _LOCKWARDEN_H_
#define _LOCKWARDEN_H_

#include "sm/sm_functions.h"

struct GaintLockGlobalWarden {
    GaintLockGlobalWarden() { InitGiantLock(); }
    ~GaintLockGlobalWarden() { DestroyGiantLock(); }
};

struct GaintLockWarden
{
    bool obtained;

    GaintLockWarden(bool immidiatelock)
      : obtained(false)
    {
        if (immidiatelock) { lock(); }
    };

    void lock()
    {
        ObtainGiantLock();
        obtained = true;
    };

    void release()
    {
        if (obtained) {
            ReleaseGiantLock();
        };
    };

    ~GaintLockWarden()
    {
        release();
    };
};

#endif /* _LOCKWARDEN_H_ */
