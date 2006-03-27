/*
 * File:  uprocess.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <stdlib.h>
#include <string.h>
#include <string>
#include "uprocess.h"
#include "d_printf.h"
#include "utils.h"

#ifdef _WIN32
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#endif


int uSetEnvironmentVariable(const char* name, const char* value)
{
#ifdef _WIN32
    BOOL res = SetEnvironmentVariable(
                  name,			// environment variable name
                  value			// new value for variable
               );
    if (res == 0) return 1;
    return 0;
#else
    int name_len = strlen(name);
    int value_len = strlen(value);
    char *str = new char[name_len + value_len + 2]; // This string will become the part 
                                                    // of the environment, so we must not delete it
    memcpy(str, name, name_len);
    str[name_len] = '=';
    memcpy(str + name_len + 1, value, value_len);
    str[name_len + value_len + 1] = '\0';
    int res = putenv(str);
    //delete [] str;
    if (res != 0) return 1;
    return 0;
#endif
}

int uGetEnvironmentVariable(const char* name, char* buf, int size)
{
#ifdef _WIN32
    memset(buf, 0, size);
    DWORD res = GetEnvironmentVariable(
                   name,		// environment variable name
                   buf,
                   size - 1
                );
    if (res == 0) return 1;
    if (res > size - 1) return 1;
    return 0;
#else
    char *res = getenv(name);
    if (res == NULL) return 1;
    if ((int)strlen(res) > size - 1) return 1;
    strcpy(buf, res);
    return 0;
#endif
}

// return value 0 indicates success
int uCreateProcess(
           char *command_line,		// command line string
           bool inherit_handles,	// handle inheritance option
           const char *cur_dir,		// current directory name
           UFlag flags,
           UPHANDLE *process_handle,
           UTHANDLE *thread_handle,
           UPID *process_id,
           UTID *thread_id,
           USECURITY_ATTRIBUTES* sa
    )
{
#ifdef _WIN32

    PROCESS_INFORMATION piProcInfo; 
    STARTUPINFO siStartInfo; 

    ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));
    ZeroMemory(&siStartInfo, sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO); 

    BOOL res = CreateProcess(
                     NULL,								// name of executable module
                     command_line,						// command line string
                     sa,								// Security attributes for the new process
                     sa,								// Security attributes for the main thread
                     inherit_handles ? TRUE : FALSE,	// handle inheritance option
                     flags,//CREATE_NEW_CONSOLE,//flags,								// creation flags
                     NULL,								// use parent's environment 
                     cur_dir,							// use parent's current directory 
                     &siStartInfo,						// STARTUPINFO pointer 
                     &piProcInfo						// receives PROCESS_INFORMATION 
               );

    if (process_handle) *process_handle = piProcInfo.hProcess;
    else CloseHandle(piProcInfo.hProcess);
    if (thread_handle) *thread_handle = piProcInfo.hThread;
    else CloseHandle(piProcInfo.hThread);

    if (process_id) *process_id = piProcInfo.dwProcessId;
    if (thread_id) *thread_id = piProcInfo.dwThreadId;

    if (res == 0) return 1;
    return 0;

#else
#define MAX_NUMBER_OF_ARGS	256
#define whitespace(c)		((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')

    pid_t pid = 0;

    if ((pid = fork()) == 0)
    { // child process

        if (flags == U_NO_WINDOW)
        {
#warning U_NO_WINDOW flag in uCreateProcess function is not implemented yet
            // close stdout and stderr to avoid output to console
            int null_dev;
            null_dev = open("/dev/null", O_RDWR);
            if (null_dev == -1) return 1;
            if (close(STDOUT_FILENO) == -1) return 1;
            if (close(STDERR_FILENO) == -1) return 1;
            if (close(STDIN_FILENO)) return 1;
            if (dup2(null_dev, STDOUT_FILENO) == -1) return 1;
            if (dup2(null_dev, STDERR_FILENO) == -1) return 1;
            if (dup2(null_dev, STDIN_FILENO) == -1) return 1;
            if (close(null_dev) == -1) return 1;
        }

        if (cur_dir != NULL)
        {
            // change current directory to cur_dir
            if (chdir(cur_dir) != 0)
            {
                d_printf1("Error changing directory\n");
                exit(1);
            }
        }

        if (!inherit_handles)
        {
#warning Only 'true' is supported as a value of inherit_handles parameter in uCreateProcess function
            // change current directory to cur_dir
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
                args[args_num] = new char[cur - pred + 1];
                args[args_num][cur - pred] = '\0';
                memcpy(args[args_num], pred, cur - pred);
                args_num++;
            }
            else break;
        }

        args[args_num] = NULL;

        if (execvp(args[0], args) == -1)
        {
            d_printf1("Error calling execvp()\n");
            exit(1);
        }

        for (args_num = 0; args[args_num] != NULL; args_num++) delete [] args[args_num];
    }
    else
    {
        if (pid == -1) return 1;

        if (process_handle) *process_handle = 0;
        if (thread_handle) *thread_handle = 0;
        if (process_id) *process_id = pid;
        if (thread_id) *thread_id = 0;
    }
    return 0;
#endif
}

int uTerminateProcess(UPID pid, UPHANDLE h, int exit_code)
{
#ifdef _WIN32
    BOOL res = TerminateProcess(
                        h, //handle to the process
                        exit_code  //exit code for the process
               );

    if (res == 0) return 1;
    return 0;     
#else
    if (kill(pid, SIGKILL) == -1) return 1;
    return 0;
#endif
}

void uExitProcess(int exit_code)
{
#ifdef _WIN32
    ExitProcess(exit_code);
#else
    exit(exit_code);
#endif
}

UPID uGetCurrentProcessId()
{
#ifdef _WIN32
    return GetCurrentProcessId();
#else
    return getpid();
#endif
}

int uIsProcessExist(UPID pid, UPHANDLE h)
{
#ifdef _WIN32
    BOOL res = FALSE;
    DWORD status = 0;
    res = GetExitCodeProcess(h, &status);
    if (res == 0)
    {
        d_printf1("GetExitCodeProcess failed\n");
        d_printf2("Error %d\n", GetLastError());
        return -1;
    }

    return (status == STILL_ACTIVE) ? 1 : 0;
#else
#ifdef HAVE_PROC
    int dsc;
    std::string path="/proc/" + int2string(pid);

    dsc = open(path.c_str(), O_RDONLY);     
    if (dsc == -1) return 0;

    close(dsc);
    return 1; 
#else
    // !!!   check for errno   !!!
    int res = kill(pid, 0);
    if (res == 0) return 1;
    else return 0;
#endif
#endif
}

int uOpenProcess(UPID pid, UPHANDLE &h)
{
#ifdef _WIN32
    h = OpenProcess(PROCESS_ALL_ACCESS, FALSE, pid); 
    if (h == NULL) return -1;
    else return 0;
#else
    return 0;
#endif
}

int uCloseProcess(UPHANDLE h)
{
#ifdef _WIN32
    int res;
    res = CloseHandle(h);
    if (res != 0) return 0;
    else return -1;
#else
    return 0;
#endif
}
/*
int uWaitForProcess(UPID pid, UPHANDLE h, bool is_child_process)
{
#ifdef _WIN32
    DWORD res;    
    res = WaitForSingleObject(h, INFINITE);

    if (res == WAIT_FAILED)
       return -1;
    else
       return 0;
#else
	int status = 0;

    if (is_child_process)
    {
       waitpid(pid, &status, 0);
    }
    else
    {
       for (;;)
       {
          status = uIsProcessExist(pid, h);
          if (status == -1) return -1;

          if (status) uSleep(1);
          else break;
       }
    }

    return 0;
#endif
}
*/
int uWaitForChildProcess(UPID pid, UPHANDLE h, int *status)
{
#ifdef _WIN32
    DWORD res1 = 0;
    BOOL res2 = FALSE;
    DWORD _status = 0;

    res1 = WaitForSingleObject(h, INFINITE);
    if (res1 == WAIT_FAILED)
        return -1;

    res2 = GetExitCodeProcess(h, &_status);
    if (res2 == 0)
        return -1;

    if (status) *status = _status;

    return 0;
#else
    int res = waitpid(pid, status, 0);
    if (res == -1) return -1;

    if (status)
    {
        if (WIFEXITED(*status))
            *status = WEXITSTATUS(*status);
        else return -1;
    }

    return 0;
#endif
}

int uWaitForProcess(UPID pid, UPHANDLE h)
{
#ifdef _WIN32
    DWORD res;    
    res = WaitForSingleObject(h, INFINITE);

    if (res == WAIT_FAILED)
       return -1;
    else
       return 0;
#else
	int status = 0;

    for (;;)
    {
       status = uIsProcessExist(pid, h);
       if (status == -1) return -1;

       if (status) uSleep(1);
       else break;
    }

    return 0;
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
int find_executable(const char *name, char *buf, int size)
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
                strncpy(buf, tbuf, size);
                return 0;
            }
        }
    }

    return -1;
}

#endif // (!defined(_WIN32) && !(defined(HAVE_PROC_EXE)))


char *program_name_argv_0 = NULL;


std::string uGetImageProcPath()
{
#ifdef _WIN32
    char buf[U_MAX_PATH+1];
    if(GetModuleFileName(0, buf, U_MAX_PATH+1) != 0)
    {
       std::string tmp = buf;
       tmp = tmp.substr(0, tmp.find_last_of('\\'));
       return tmp;
    }
    else 
      return "";     
#else
#ifdef HAVE_PROC_EXE
    std::string link="/proc/" + int2string(getpid()) + PROC_EXE_SUFFIX;

    int len;
    char buf[U_MAX_PATH+1];

    len = readlink(link.c_str(), buf, U_MAX_PATH);
    if (len == -1) return "";
	
    buf[len] = '\x0';
    std::string tmp = buf;

    tmp = tmp.substr(0, tmp.find_last_of('/'));
    return tmp;
#else
    char buf[U_MAX_PATH+1];
    if (find_executable(program_name_argv_0, buf, U_MAX_PATH+1) == 0)
    {
        std::string tmp = buf;
        tmp = tmp.substr(0, tmp.find_last_of('/'));
        return tmp;
    }
    else 
      return "";     
#endif
#endif
}

