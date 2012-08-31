#ifndef  _SHMWARDEN_H_
#define  _SHMWARDEN_H_

#include "common/sedna.h"
#include "u/ushm.h"

class SharedMemoryAttachWarden
{
    UShMem shmem;
    global_name gname;
    char * _data;
public:
    SharedMemoryAttachWarden(global_name _gname) : gname(_gname)
    {
        if (uOpenShMem(&shmem, gname, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE4016);
        };

        if (NULL == (_data = (char *) uAttachShMem(&shmem, NULL, 0, __sys_call_error))) {
            throw USER_EXCEPTION(SE4023);
        }
    };

    ~SharedMemoryAttachWarden() {
        if (uDettachShMem(&shmem, _data, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4024);

        if (uCloseShMem(&shmem, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4020);
    }

    char * data() { return _data; };
};


class SharedMemoryWarden
{
    UShMem shmem;
    global_name gname;
    char * _data;
public:
    SharedMemoryWarden(global_name _gname, size_t sz) : gname(_gname)
    {
        if (uCreateShMem(&shmem, gname, sz, NULL, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE4016);
        };

        if (NULL == (_data = (char *) uAttachShMem(&shmem, NULL, 0, __sys_call_error))) {
            throw USER_EXCEPTION(SE4023);
        }
    };

    ~SharedMemoryWarden() {
        if (uDettachShMem(&shmem, _data, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4024);

        if (uReleaseShMem(&shmem, gname, __sys_call_error) != 0)
            throw USER_EXCEPTION(SE4020);
    }

    char * data() { return _data; };
};

#endif /* _SHMWARDEN_H_ */
