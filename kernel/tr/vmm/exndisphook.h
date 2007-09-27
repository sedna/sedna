#if _MSC_VER > 1000
#pragma once
#endif

#ifndef EXNDISPHOOK_H_INCLUDED
#define EXNDISPHOOK_H_INCLUDED

#ifndef __cplusplus
#define EXTERNC
#else
#define EXTERNC extern "C"
#endif

typedef
LONG (NTAPI *UserExceptionDispatcherProc)(PEXCEPTION_RECORD ExceptionRecord, PCONTEXT Context);

EXTERNC int InstallKiUserExceptionDispatcherHook(UserExceptionDispatcherProc hook);

#endif
