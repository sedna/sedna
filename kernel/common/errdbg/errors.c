#include "errors.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#endif

#include "u/u.h"

#include "common/errdbg/event_log.h"
#include "common/errdbg/d_printf.h"

void __sys_call_error(const char *filename, int lineno, const char *funcname, const char *sys_call, const void* arg)
{
#if !(defined(SE_NO_EVENT_LOG))
    char buf[256];
    int code = uerrno(funcname, arg);
#endif

    d_perror(sys_call);

#if !(defined(SE_NO_EVENT_LOG))
    ustrerror_r(code, buf, 256);
    event_log_short_msg(EL_SYS,
                        filename,
                        lineno,
                        funcname,
                        "%s (code = %d): %s",
                        sys_call,
                        code,
                        buf);
#endif
}

/* Simply writes message to the event log
 * Intended to be used inside u-functions to
 * write additional error condition information.
 */
void
 __u_call_error(const char *filename,
               int lineno,
               const char *funcname,
               const char *message)
{
#if !(defined(SE_NO_EVENT_LOG))
    event_log_short_msg(EL_ERROR,
                        filename,
                        lineno,
                        funcname,
                        message);
#endif /* !SE_NO_EVENT_LOG */

}

void DumpFaultInfo()
{
        int processId = -1, bIsTrunc = 0;
        const char *cmdline = "<failed to obtain cmdline>";
#ifdef _WIN32
        /* very simple, isn't it? */
        const char *cmdline2 = NULL;
        processId = (int)GetCurrentProcessId();
        cmdline2 = GetCommandLine();
        if (cmdline2) cmdline = cmdline2;
#else
        /* rather bloat
 *              - read command line from /proc/[PID]/cmdline
 *              - reconstruct command line (we get it already tokenized)
 *              - certain characters are escaped during reconstruction
 *              - operating on fixed size buffers thus having to truncate
 *                resulting string gracefully when ran out of buffer space */
        pid_t pid = 0;
        int fd = 0, bNeedQuotes = 0;
        size_t sz = 0;
        char path[64];
        char buf[256], obuf[256];
        char *a = NULL, *b = NULL, *aEnd = NULL, *bEnd = NULL;

        pid = getpid();
        sz = snprintf(path, sizeof path, "/proc/%d/cmdline", (int)pid);
        if (sz==-1 || sz>=sizeof path)
        {
                /* buffer too small to hold the path */
        }
        else if (fd = open(path, O_RDONLY), fd==-1)
        {
                /* proc filesystem n/a */
        }
        else
        {
                a = buf;
                aEnd = buf + sizeof(buf) - 1; /* space for \0 terminator */
                b = obuf;
                bEnd = obuf + sizeof (obuf) - 2; /* space for extra char & \0 */

                for (sz=1; a!=aEnd && sz>0; )
                {
                        /* have one extra byte after aEnd, to identify trunc */
                        sz = read(fd, a, aEnd - a + 1);
                        if (sz==-1)
                        {
                                if (errno == EINTR) sz=1; /* not an error */
                        }
                        else a+=sz;
                }
                close(fd);
                /* read successfully? */
                if (sz!=-1)
                {
                        bIsTrunc = (a>aEnd);
                        if (!bIsTrunc) aEnd = a;
                        a=buf; *aEnd=0;
                        /* processing tokens one-by-one (\0 terminated)*/
                        while (a<aEnd && b<bEnd)
                        {
                                /* processing token */
                                bNeedQuotes = (*a==0); /* when token is "" */
                                if (bNeedQuotes) b++[0]='\"';
                                while (*a && b<bEnd)
                                {
                                        switch (*a)
                                        {
                                        /* escape special chars */
                                        case ' ':
                                        case '\\':
                                        case '\"':
                                                /* we have 1 extra char
 *                                                 after bEnd, so this is
 *                                                 valid */
                                                b++[0] = '\\';
                                        }
                                        b++[0] = *a;
                                        ++a;
                                }
                                bIsTrunc |= (bNeedQuotes && b>=bEnd);
                                if (b<bEnd && bNeedQuotes) b++[0]='\"';
                                /* a+1 points to next token, if one exists */
                                if (a+1<aEnd && b<bEnd) b++[0]=' ';
                                /* if we discard ' ' due to truncation we will
 *                                 leave outter loop prematurely and bIsTrunc
 *                                 flag is set outside (not all tokens
 *                                 processed)  */
                                bIsTrunc |= (*a!=0);
                                ++a;
                        }
                        bIsTrunc |= (a<aEnd); /* not all tokens processed */
                        bEnd = b; b=obuf; *bEnd=0;
                        cmdline = obuf;
                }
                processId = (int)pid;
        }
#endif
        assert (cmdline);
        fprintf(stderr, "in process [%d] %s%s\n",
                        processId, cmdline,
                        bIsTrunc ? "...<truncated>":"");
};

