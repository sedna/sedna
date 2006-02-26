/*
 * File:  udl.h
 * Copyright (C) 2005 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _U_DL_H
#define _U_DL_H

#ifdef _WIN32

typedef HMODULE ULibrary;
#define UDL_INVALID_LIBRARY		NULL

#define uLoadLibrary(fname)				LoadLibrary(fname)
#define uFreeLibrary(lib)				FreeLibrary(lib)
#define uGetProcAddress(lib,sym)		GetProcAddress(lib,sym)

#else //UNIX

#include <dlfcn.h>

typedef void * ULibrary;
#define UDL_INVALID_LIBRARY		NULL

#define uLoadLibrary(fname)			dlopen(fname, RTLD_LAZY)
#define uFreeLibrary(lib)			dlclose(lib)
#define uGetProcAddress(lib,sym)	dlsym(lib,sym)

#endif

#endif //_U_DL_H
