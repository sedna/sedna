/*
 * File:  upipe.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _UPIPE_H
#define _UPIPE_H

#include <string>
#include "common/u/u.h"


#ifdef _WIN32
typedef HANDLE	UPIPE;
#else
typedef int	UPIPE;
#endif

// return value 0 indicates success
int uPipe(UPIPE* rpipe,		/*read pipe*/
          UPIPE* wpipe,		/*write pipe*/
          int inheritable,	/*is handles inhereted by child processes*/
          sys_call_error_fun fun
         );

// positive return values indicates number of bytes read
// if return value is 0 then end of file is reached
// negative return values indicates an error
int uReadPipe(UPIPE rpipe,	/*read pipe*/
              void *buf,	/*buffer for data*/
              int nbyte,		/*size of the buffer*/
              sys_call_error_fun fun
             );

// positive return values indicates number of bytes read
// if return value is 0 then end of file is reached
// negative return values indicates an error
int uReadPipeAll(UPIPE rpipe, // read pipe
                 void* buf,   // buffer for data
                 int nbyte,    // size of the buffer
                 sys_call_error_fun fun
                );


// reads a string from pipe (reads until \0 symbol meet)
int uReadPipeMsg(UPIPE rpipe,std::string &str, sys_call_error_fun fun);

// return value indicates number of bytes actually written to the pipe
// negative return value indicates error
int uWritePipe(UPIPE wpipe,	/*write pipe*/
               const void *buf,	/*pointer to data*/
               int nbyte,	/*size of the data*/
               sys_call_error_fun fun
              );

// return value 0 indicates success
int uWritePipeAll(UPIPE wpipe,	/*write pipe*/
                  const void *buf,	/*pointer to data*/
                  int nbyte,	/*size of the data*/
                  sys_call_error_fun fun
                 );

// return value 0 indicates success
int uClosePipe(UPIPE upipe, sys_call_error_fun fun);

// return value 0 indicates success
int uReadPipeAll(UPIPE rpipe, std::string &str, sys_call_error_fun fun);

// return value 0 indicates success
int uPipeDoNotInherit(UPIPE *upipe, sys_call_error_fun fun);

#endif

