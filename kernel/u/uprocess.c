/*
 * File:  uprocess.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "u/uprocess.h"
#include "common/errdbg/d_printf.h"
#include "u/uutils.h"

#ifdef _WIN32
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#endif



/* Change or add an environment variable.
 * name   (in)  - name of the varilable to add or change
 * value  (in)  - value of variable to set
 * buffer (out) - if not NULL then *nix putenv() semantics is 
 *                used, so that address of the variable's value
 *                is saved in this argument.
 *                It's your task to clean up this memory with 
 *                free() call. On Windows NULL is returned.
 * Returns:
 * 0 - in case of success,
 * 1 - in case of error.
 */
int uSetEnvironmentVariable(const char* name, const char* value, char** buffer, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res;
    if(buffer) *buffer = NULL;
    res = SetEnvironmentVariable(
                  name,         /* environment variable name */
                  value);       /* new value for variable */

    if (res == 0) {
       sys_call_error("SetEnvironmentVariable");
       return 1;
    }
    return 0;
#else
    size_t name_len  = strlen(name);
    size_t value_len = strlen(value);

    /* This string will become the part 
     * of the environment, so we must not delete it there.
     */
    char *str = (char*)malloc(name_len + value_len + 2);
    if(buffer) *buffer = str;

    memcpy(str, name, name_len);
    str[name_len] = '=';
    memcpy(str + name_len + 1, value, value_len);
    str[name_len + value_len + 1] = '\0';
    int res = putenv(str);

    if (res != 0) {
       sys_call_error("putenv");
       return 1;
    }
    
    return 0;
#endif
}

/*
 * Gets the of the environment variable specified by the name.
 * Puts it into buffer of the defined size.
 * If the function succeeds, it returns 0; otherwise it retunrs non-zero
 * value (inclusing situation when there is not enough space in buffer 
 * to place the value.
 */
int uGetEnvironmentVariable(const char* name, char* buf, uint32_t size, sys_call_error_fun fun)
{
#ifdef _WIN32
    DWORD res;
    memset(buf, 0, size);
    res = GetEnvironmentVariable(
             name,      /* environment variable name */
             buf,       /* buffer to place the value*/
             size - 1); /* buffer size*/
    if (res == 0)
    {
       sys_call_error("GetEnvironmentVariable");
       return 1;
    }
    if (res > size - 1) return 1;
    return 0;
#else
    char *res = getenv(name);
    if (res == NULL)
    {
      sys_call_error("getenv");
      return 1;
    }
    if ((uint32_t)strlen(res) > size - 1) return 1;
    strcpy(buf, res);
    return 0;
#endif
}

int uGetCurProcessWorkingSetSize(size_t *MinWorkingSetSize, size_t *MaxWorkingSetSize, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = GetProcessWorkingSetSize(
                  GetCurrentProcess(), // handle to the process
                  MinWorkingSetSize,   // minimum working set size
                  MaxWorkingSetSize    // maximum working set size
               );
    if (res == 0)
    {
        sys_call_error("GetProcessWorkingSetSize");
        return 1;
    }
    return 0;
#else
    *MinWorkingSetSize = *MaxWorkingSetSize = 0;
    return 0;
#endif
}

int uSetCurProcessWorkingSetSize(size_t MinWorkingSetSize, size_t MaxWorkingSetSize, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = SetProcessWorkingSetSize(
                  GetCurrentProcess(), // handle to the process
                  MinWorkingSetSize,   // minimum working set size
                  MaxWorkingSetSize    // maximum working set size
               );
    if (res == 0)
    {
        sys_call_error("SetProcessWorkingSetSize");
        return 1;
    }
    return 0;
#else
    return 0;
#endif
}

/* return value 0 indicates success */
int uCreateProcess(
           char *command_line,		/* command line string */
           bool inherit_handles,	/* handle inheritance option (not implem*/
           const char *cur_dir,		/* current directory name */
           UFlag flags,
           UPHANDLE *process_handle,
           UTHANDLE *thread_handle,
           UPID *process_id,
           UTID *thread_id,
           USECURITY_ATTRIBUTES* sa,
           sys_call_error_fun fun
    )
{
#ifdef _WIN32

    PROCESS_INFORMATION piProcInfo; 
    STARTUPINFO siStartInfo; 
    BOOL res;

    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));
    ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO); 

    if(CREATE_NEW_CONSOLE == flags)
    { 
        siStartInfo.dwFlags = STARTF_USESHOWWINDOW;
        siStartInfo.wShowWindow = SW_HIDE;
    }

    res = CreateProcess(
                NULL,								/* name of executable module */
                command_line,						/* command line string */
                sa,									/* Security attributes for the new process */
                sa,									/* Security attributes for the main thread */
                inherit_handles ? TRUE : FALSE,		/* handle inheritance option */
                flags,                              /* creation flags */
                NULL,								/* use parent's environment */
                cur_dir,							/* use parent's current directory */
                &siStartInfo,						/* STARTUPINFO pointer */
                &piProcInfo						    /* receives PROCESS_INFORMATION */
          );

    if (res == 0) sys_call_error("CreateProcess");

    if (process_handle) *process_handle = piProcInfo.hProcess;
    else if (CloseHandle(piProcInfo.hProcess) == 0) sys_call_error("CloseHandle");

    if (thread_handle) *thread_handle = piProcInfo.hThread;
    else if (CloseHandle(piProcInfo.hThread) == 0) sys_call_error("CloseHandle");

    if (process_id) *process_id = piProcInfo.dwProcessId;
    if (thread_id) *thread_id = piProcInfo.dwThreadId;

    if (res == 0) return 1;
    return 0;

#else
#define MAX_NUMBER_OF_ARGS	256
#define whitespace(c)		((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')

    pid_t pid = 0;

    if ((pid = fork()) == 0)
    { /* child process */

        if (flags == U_DETACHED_PROCESS)
        {
            /* close stdout and stderr to avoid output to console */
            int null_dev = open("/dev/null", O_RDWR);
            if (null_dev == -1) { sys_call_error("open"); exit(1); }

            if (close(STDOUT_FILENO) == -1) { sys_call_error("close"); exit(1); }
            if (close(STDERR_FILENO) == -1){ sys_call_error("close"); exit(1); }
            if (close(STDIN_FILENO) == -1) { sys_call_error("close");  exit(1); }
           
            if (dup2(null_dev, STDOUT_FILENO) == -1) { sys_call_error("dup2"); exit(1); }
            if (dup2(null_dev, STDERR_FILENO) == -1) { sys_call_error("dup2"); exit(1); }
            if (dup2(null_dev, STDIN_FILENO) == -1) { sys_call_error("dup2"); exit(1); }
            
            if (close(null_dev) == -1) { sys_call_error("close"); exit(1); }
        }

        if (cur_dir != NULL)
        {
            /* change current directory to cur_dir */
            if (chdir(cur_dir) != 0)
            {
                sys_call_error("chdir");
                exit(1);
            }
        }

        char *args[MAX_NUMBER_OF_ARGS];
        memset(args, '\0', MAX_NUMBER_OF_ARGS * sizeof(char*));
        int args_num = 0;

        char *pred = NULL;
        char *cur = command_line;

        while (true)
        {
            while (whitespace(*cur)) cur++;

            pred = cur;

            while (!(whitespace(*cur) || !*cur)) cur++;

            if (pred < cur)
            {
                args[args_num] = (char*)malloc(cur - pred + 1);
                args[args_num][cur - pred] = '\0';
                memcpy(args[args_num], pred, cur - pred);
                args_num++;
            }
            else break;
        }

        args[args_num] = NULL;
        
        if (execvp(args[0], args) == -1)
        {
            sys_call_error("execvp");
            exit(1);
        }

        for (args_num = 0; args[args_num] != NULL; args_num++) free(args[args_num]);
    }
    else
    {
        if (pid == -1)
        {
          sys_call_error("fork");
          return 1;
        }

        if (process_handle) *process_handle = 0;
        if (thread_handle) *thread_handle = 0;
        if (process_id) *process_id = pid;
        if (thread_id) *thread_id = 0;
    }
    return 0;
#endif
}

int uTerminateProcess(UPID pid, UPHANDLE h, int exit_code, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = TerminateProcess(
                        h,         /*handle to the process */
                        exit_code  /* exit code for the process */
               );

    if (res == 0) 
    {
      sys_call_error("TerminateProcess");
      return 1;
    }
    return 0;     
#else
    if (kill(pid, SIGKILL) == -1)
    {
      sys_call_error("kill");
      return 1;
    }
    return 0;
#endif
}

void uExitProcess(int exit_code, sys_call_error_fun fun)
{
#ifdef _WIN32
    ExitProcess(exit_code);
#else
    exit(exit_code);
#endif
}

UPID uGetCurrentProcessId(sys_call_error_fun fun)
{
#ifdef _WIN32
    return GetCurrentProcessId();
#else
    return getpid();
#endif
}

/* Check if process exists.
 * Return values:
 *  0 - process alive
 * -1 - process doesn't exist
 * -2 - some error occurred
 * 
 * At present uOpenProcess is implemented via this function on
 * any Nix like OS. 
 *
 * If pid == 0 fuction always returns -1 for compatibility 
 * between POSIX-like operating systems. For example, FreeBSD 
 * has process with PID=0, Linux doesn't.  
 */
int 
uIsProcessExist(UPID pid, UPHANDLE h, sys_call_error_fun fun) {
#ifdef _WIN32
    BOOL res = FALSE;
    DWORD status = 0;

    if (pid == 0) return -1;

    res = GetExitCodeProcess(h, &status);
    if (res == 0) {
        sys_call_error("GetExitCodeProcess");
        return -2;
    }

    return (status == STILL_ACTIVE) ? 0 : -1;
#else
    int res;

#ifdef HAVE_PROC /* Linux, SunOS */

    char buf[U_MAX_PATH];
    struct stat stf;

    if (pid == 0) return -1;

    strcpy(buf, "/proc/");
    int2c_str(pid, buf + strlen("/proc/"));

    /* Attempt to get the file attributes */
    res = stat(buf, &stf);

    /* On SunOS we have to handle EINTR appropriately */
    while (-1 == res) {
        /* There is no such file */
        if ( ENOENT == errno ) 
            return -1; 
    #if defined(SunOS)        
        else if( EINTR == errno )
            res = stat(buf, &stf);
    #endif
        else {
            sys_call_error("stat");
            return -2;
        }
    }

    return 0;

#else /* !HAVE_PROC: Darwin, FreeBSD */

    if (pid == 0) return -1;
    
    res = kill(pid, 0);
    
    if(-1 == res) {
        /* Process exists but we don't have permissions 
         * to send a signal.
         */
        if(EPERM == errno) return 0;
        
        /* The pid doesn't exist. */
        return -1;
    }
    else {
        /* Process exists. */
        return 0;
    }

#endif /* HAVE_PROC */
#endif /* _WIN32 */
}

/* Win: Opens an existing local process object with
 *      SYNCHRONIZE privilege.
 * Nix: Simply checks out if process exists.
 * Return values:
 *  0 - process successfully opened
 * -1 - some error occured or process doesn't exist
 */
int 
uOpenProcess(UPID pid, UPHANDLE *h, sys_call_error_fun fun) {
#ifdef _WIN32
    /* SYNCHRONIZE enough since we want just to call
       WaitForSingleObject fuction on this handle */
    *h = OpenProcess(SYNCHRONIZE, FALSE, pid); 
    if (*h == NULL) {
        sys_call_error("OpenProcess"); 
        return -1;
    }
    else 
        return 0;
#else
    /*On NIX we don't need process handle.
      So just check if process exists.
     */
    int res = uIsProcessExist(pid, 0, fun);
    *h = 0;
    return (0 == res) ? 0 : -1;
#endif
}


int uCloseProcessHandle(UPHANDLE h, sys_call_error_fun fun)
{
#ifdef _WIN32
    int res;
    res = CloseHandle(h);
    if (res != 0) return 0;
    else 
    {
       sys_call_error("CloseHandle");  
       return -1;
    }
#else
    return 0;
#endif
}

int uWaitForChildProcess(UPID pid, UPHANDLE h, int *status, sys_call_error_fun fun)
{
#ifdef _WIN32
    DWORD res1 = 0;
    BOOL res2 = FALSE;
    DWORD _status = 0;

    res1 = WaitForSingleObject(h, INFINITE);
    if (res1 == WAIT_FAILED)
    {
        sys_call_error("WaitForSingleObject");
        return -1;
    }

    res2 = GetExitCodeProcess(h, &_status);
    if (res2 == 0)
    {
       sys_call_error("GetExitCodeProcess");
       return -1;
    }

    if (status) *status = _status;

    return 0;
#else
    int res = waitpid(pid, status, 0);
    if (res == -1)
    {
      sys_call_error("waitpid");
      return -1;
    }

    if (status)
    {
        if (WIFEXITED(*status))
            *status = WEXITSTATUS(*status);
        else return -1;
    }

    return 0;
#endif
}

/*
 * Waits for process specified by PID (by HANDLE on Win32 API) to
 * be finished. On Windows HANDLE must be with SYNCHRONIZE privilege
 * enabled. We request it via OpenProcess or automatically get if
 * process was created with CreateProcess.
 */
int uWaitForProcess(UPID pid, UPHANDLE h, sys_call_error_fun fun)
{
#ifdef _WIN32
    DWORD res;    
    res = WaitForSingleObject(h, INFINITE);

    if (res == WAIT_FAILED)
    {
       sys_call_error("WaitForSingleObject");
       return -1;
    }
    else
       return 0;
#else
	int status = 0;

    for (;;)
    {
       status = uIsProcessExist(pid, h, __sys_call_error);
       /* Error happened in uIsProcessExist */
       if (-2 == status) return -1;
       /* Process still alive */
       if ( 0 == status) uSleep(1, __sys_call_error);
       /* status == -1, there is no such process */
       else break;
    }

    return 0;
#endif
}

int uNonBlockingWaitForChildProcess(UPID pid)
{
#ifdef _WIN32
    return 0;
/// Nothing doing here
#else
    int status = 0;

    int res = waitpid(pid, &status, WNOHANG);

    return res;
#endif
}

#if (!defined(_WIN32) && !(defined(HAVE_PROC_EXE)))

#include <pwd.h>


/* ABSOLUTE_FILENAME_P (fname): True if fname is an absolute filename */
#ifdef atarist
#define ABSOLUTE_FILENAME_P(fname)	((fname[0] == '/') || \
	(fname[0] && (fname[1] == ':')))
#else
#define ABSOLUTE_FILENAME_P(fname)	(fname[0] == '/')
#endif /* atarist */

/* Return the absolute name of the program named NAME.  This function
   searches the directories in the PATH environment variable if PROG
   has no directory components. */
static
int find_executable(const char *name, char *buf, size_t size)
{
    char *search;
    char *p;
    char tbuf[MAXPATHLEN];

    if (ABSOLUTE_FILENAME_P(name)) 
    {
        /* If we can execute the named file, and it is normal, then return it. */
        if (!access (name, X_OK))
        {
	        struct stat stat_temp;

            if (stat (name, &stat_temp))
                return -1;

#ifndef STAT_MACROS_BROKEN
            if (!S_ISREG(stat_temp.st_mode))
                return -1;
#endif
            strncpy(buf, name, size);
            return 0; 
        }
    }
    else 
    {
        if ((name[0] == '.') && (name[1] == '/'))
        {
            strcpy(tbuf, ".");

#ifdef HAVE_GETCWD
            getcwd(tbuf, MAXPATHLEN);
#else
# ifdef HAVE_GETWD
            getwd(tbuf);
# endif
#endif
            strcat(tbuf, name + 1);
            strncpy(buf, tbuf, size);
            return 0;
        }

        if ((name[0] == '.') && (name[1] == '.') && (name[2] == '/'))
        {

#ifdef HAVE_GETCWD
            getcwd(tbuf, MAXPATHLEN);
            strcat(tbuf, "/");
#else
# ifdef HAVE_GETWD
            getwd(tbuf);
            strcat(tbuf, "/");
# endif
#endif
            strcat(tbuf, name);
            strncpy(buf, tbuf, size);
            return 0;
        }

        search = getenv("PATH");
        p = search;

        while (*p)
        {
            char *next;
            next = tbuf;

            /* Perform tilde-expansion. Stolen from GNU readline/tilde.c. */
            if (p[0] == '~') 
            {
                if (!p[1] || p[1] == '/') 
                {
                    /* Prepend $HOME to the rest of the string. */
                    char *temp_home = (char*)getenv("HOME");

                    /* If there is no HOME variable, look up the directory in
                       the password database. */
                    if (!temp_home) 
                    {
                        struct passwd *entry;

                        entry = getpwuid(getuid());
                        if (entry)
                             temp_home = entry->pw_dir;
                    }

                    strcpy(tbuf, temp_home);
                    next = tbuf + strlen(tbuf);
                    p++;
                }
                else
                {
                    char username[MAXPATHLEN];
                    struct passwd *user_entry;
                    int i;

                    p++; /* Skip the tilde. */
                    for (i = 0; *p && *p != '/' && *p != ':'; i++)
                        username[i] = *p ++;
                    username[i] = '\0';

                    user_entry = getpwnam(username);
                    if (user_entry) 
                    {
                        strcpy(tbuf, user_entry->pw_dir);
                        next = tbuf + strlen(tbuf);
                    }
                }

                endpwent();
            }

            /* Copy directory name into [tbuf]. */
            while (*p && *p != ':')
                *next ++ = *p ++;
            *next = '\0';
            if (*p != '\0')
            p++;

            if (tbuf[0] == '.' && tbuf[1] == '\0') 
            {
#ifdef HAVE_GETCWD
                getcwd (tbuf, MAXPATHLEN);
#else
# ifdef HAVE_GETWD
                getwd (tbuf);
# endif
#endif
            }

            strcat(tbuf, "/");
            strcat(tbuf, name);

            /* If we can execute the named file, and it is normal, then return it. */
            if (!access(tbuf, X_OK)) 
            {
                struct stat stat_temp;

                if (stat(tbuf, &stat_temp))
                    continue;

#ifndef STAT_MACROS_BROKEN
                if (!S_ISREG(stat_temp.st_mode))
                    continue;
#endif
                if(realpath(tbuf, buf) != NULL)
                    return 0;            
            }
        }
    }

    return -1;
}

#endif /* (!defined(_WIN32) && !(defined(HAVE_PROC_EXE))) */


char *program_name_argv_0 = NULL;


char* uGetImageProcPath(char *buf, sys_call_error_fun fun)
{
#ifdef _WIN32
    char *p = buf;
    if (GetModuleFileName(0, buf, U_MAX_PATH + 1) != 0)
    {
        p = strrchr(buf, '\\');
        if (!p) p = buf;
    }
    else
       sys_call_error("GetModuleFileName");

    *p = '\0';
    return buf;
#else
#ifdef HAVE_PROC_EXE
    char *p = buf;
    int len = 0;
    char tmp[U_MAX_PATH + 1];

    strcpy(tmp, "/proc/");
    int2c_str(getpid(), tmp + strlen("/proc/"));
    strcat(tmp, PROC_EXE_SUFFIX);

    len = readlink(tmp, buf, U_MAX_PATH);
    if (len != -1)
    {
        buf[len] = '\0';
        p = strrchr(buf, '/');
        if (!p) p = buf;
    }
    else sys_call_error("uGetImageProcPath");
    
    *p = '\0';
    return buf;
#else
    char *p = buf;
    if (find_executable(program_name_argv_0, buf, U_MAX_PATH + 1) == 0)
    {
        p = strrchr(buf, '/');
        if (!p) p = buf;
    }

    *p = '\0';
    return buf;
#endif
#endif
}

