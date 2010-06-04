/*
 * File:  upipe.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/errdbg/d_printf.h"
#include "common/u/upipe.h"

int uPipe(UPIPE* rpipe, /*read pipe*/
          UPIPE* wpipe, /*write pipe*/
          int inheritable,
          sys_call_error_fun fun)
{
#ifdef _WIN32
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = inheritable ? TRUE : FALSE;

    BOOL res = CreatePipe(
                     rpipe,		// read handle
                     wpipe,		// write handle
                     &sa,		// security attributes
                     0			// pipe size
               );

    if (res != 0) return 0;
    else
    {
      sys_call_error("CreatePipe");
      return -1;
    }
#else
    int fildes[2];
    int res = pipe(fildes);
    if (res == -1) sys_call_error("pipe");

    *rpipe = fildes[0];
    *wpipe = fildes[1];
    return res;
#endif
}

int uReadPipe(UPIPE rpipe,	/*read pipe*/
              void *buf,	/*buffer for data*/
              int nbyte,	/*size of the buffer*/
              sys_call_error_fun fun
             )
{
#ifdef _WIN32
    DWORD bytes_read = 0;
    BOOL res = ReadFile(
                   rpipe,		// handle to file
                   buf,			// data buffer
                   nbyte,		// number of bytes to read
                   &bytes_read,		// number of bytes read
                   NULL			// overlapped buffer
               );

    if (res == 0) 
    {
        if (GetLastError() == 109) return bytes_read;
        else
        {
            sys_call_error("ReadFile");
            return -1;
        }
    }
    else return bytes_read;
#else
    int res;
    if ((res = read(rpipe, buf, nbyte)) == -1)
       sys_call_error("read");

    return res;
#endif
}

//return the number of bytes read
// -1 indicates failure
int uReadPipeAll(UPIPE rpipe, // read pipe
                 void* buf,   // buffer for data
                 int nbyte,    // size of the buffer
                 sys_call_error_fun fun
                )
{
    int res = 0;
    int bytes_read = 0;
    while (true)
    {
        res = uReadPipe(rpipe, (char*)buf + bytes_read, nbyte - bytes_read, __sys_call_error);
        if (res == -1) return -1;
  
        if (res == 0 ) return bytes_read;
        else bytes_read += res;

        if (bytes_read == nbyte) return bytes_read;

    }
}


/*
int uReadPipeMsg(UPIPE rpipe,
	             std::string &str, sys_call_error_fun fun)
{
    char buf[2];
    DWORD bytes_read;
    str = "";
    for (;;)
    {
        memset(buf, (char)0, 1);
  	    BOOL res = ReadFile(
                    rpipe,		// handle to file
                    buf,			// data buffer
                    1,		    // number of bytes to read
                    &bytes_read,		// number of bytes read
                    NULL			// overlapped buffer
                    );
        if (bytes_read < 0) return -1;
        if (buf[0] == '\0') break;
        
        buf[1]='\0';                      
        str += std::string(buf);
    }
    
    return 0;
}
*/

int uWritePipe(UPIPE wpipe,	/*write pipe*/
               const void *buf,	/*pointer to data*/
               int nbyte,	/*size of the data*/
               sys_call_error_fun fun
              )
{
#ifdef _WIN32
    DWORD bytes_written = 0;
    BOOL res = WriteFile(
                    wpipe,		// handle to file
                    buf,		// data buffer
                    nbyte,		// number of bytes to write
                    &bytes_written,	// number of bytes written
                    NULL		// overlapped buffer
               );

    if (res == 0) 
    {
        sys_call_error("WriteFile");
        return -1;
    }
    else return bytes_written;
#else
    int res;
    if ((res = write(wpipe, buf, nbyte)) == -1)
       sys_call_error("write");
 
    return res;
#endif
}

// return value 0 indicates success
int uWritePipeAll(UPIPE wpipe,	/*write pipe*/
                  const void *buf,	/*pointer to data*/
                  int nbyte,	/*size of the data*/
                  sys_call_error_fun fun
                 )
{
    int res = 0;
    int bytes_written = 0;
    while (true)
    {
        res = uWritePipe(wpipe, (char*)buf + bytes_written, nbyte - bytes_written, __sys_call_error);
        if (res == -1) return -1;
        else bytes_written += res;

        if (bytes_written == nbyte) return 0;
    }
}

int uClosePipe(UPIPE upipe, sys_call_error_fun fun)
{
#ifdef _WIN32
    BOOL res = CloseHandle(
                    upipe		// handle to object
               );


    if (res == 0)
    {
       sys_call_error("CloseHandle");
       return -1;
    }
    else return 0;
#else
    int res;
    if ((res = close(upipe)) == -1)
       sys_call_error("close");

    return res;
#endif
}

#define BUFSIZE 1024

int uReadPipeAll(UPIPE rpipe, std::string &str, sys_call_error_fun fun)
{
    char buf[BUFSIZE + 1];
    int bytes_read;
    for (;;)
    {
        memset(buf, (char)0, BUFSIZE + 1);
        bytes_read = uReadPipe(rpipe, buf, BUFSIZE, __sys_call_error);
        if (bytes_read < 0) return -1;
        if (bytes_read == 0) break;
        str += std::string(buf);
    }
    return 0;
}

/*
int uPipeDoNotInherit(UPIPE *upipe, sys_call_error_fun fun)
{
#ifdef _WIN32
    HANDLE tmp = *upipe;
    BOOL res = DuplicateHandle(
                        GetCurrentProcess(), 
                        tmp,
                        GetCurrentProcess(), 
                        upipe, 
                        0,
                        FALSE,
                        DUPLICATE_SAME_ACCESS);
    
    CloseHandle(tmp);

    if (res == 0) return -1;
    else return 0;
#else
    return 0;
#endif
}
*/
