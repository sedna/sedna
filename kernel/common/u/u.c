#include "u.h"
#include "d_printf.h"

#ifdef _WIN32
#else
#include <unistd.h>
#include <fcntl.h>
#endif


int uNotInheritDescriptor(UHANDLE h)
{
#ifdef _WIN32
    if (SetHandleInformation(h, HANDLE_FLAG_INHERIT, 0) == 0)
        return -1;
    else
        return 0;
#else

    if (fcntl(h, F_SETFD, FD_CLOEXEC) == -1)
    {
        d_perror("fcntl (1)");
        return -1;
    }
    else
        return 0;
#endif
}
