#define _WIN32_WINNT 0x0500 
#define VC_EXTRALEAN 
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "detours.h"
#include "exndisphook.h"

INT NTAPI NtContinue(PCONTEXT ThreadContext, BOOLEAN RaiseAlert);

VOID NTAPI KiUserExceptionDispatcher(PEXCEPTION_RECORD ExceptionRecord, PCONTEXT Context);

static FARPROC kiUserExceptionDispatcherHook = NULL;

static FARPROC kiUserExceptionDispatcher = NULL;

__declspec(naked)
static VOID __hooked__KiUserExceptionDispatcher(PEXCEPTION_RECORD ExceptionRecord, PCONTEXT Context)
{	
	/*	KiUserExceptionDispatcher has a custom calling convention. It passes parameters like
		__stdcall functions but return address is never pushed on stack since the function never
		returns. */ 
	__asm mov eax, [esp]; 
	__asm mov ebx, [esp+4];
	__asm sub esp, 8;
	__asm mov [esp], eax;
	__asm mov [esp+4], ebx;
	__asm call [kiUserExceptionDispatcherHook];
	/* check if hook requested to continue execution */ 
	__asm cmp eax, 0xffffffff;	
	__asm jne label0;
	__asm mov eax, [esp+4];
	__asm xor ebx, ebx;
	__asm sub esp, 8;
	__asm mov [esp], eax;
	__asm mov [esp+4], ebx;
	__asm call NtContinue;
label0:
	__asm jmp [kiUserExceptionDispatcher];
}

int InstallKiUserExceptionDispatcherHook(UserExceptionDispatcherProc hook)
{
	HMODULE hNtdllModule = NULL;
	FARPROC targetProc = NULL;
	int success = 0;
	
	if (hook==NULL)
	{
		/*	Invalid hook! */ 
	}
	if (kiUserExceptionDispatcher!=NULL)
	{
		/*	KiUserExceptionDispatcher already hooked, just update hook pointer. */ 
		kiUserExceptionDispatcherHook = (FARPROC)hook;
		success = 1;
	}
	else 
	{
		/*	Though we have KiUserExceptionDispatcher callable via the import library,  
			we MUST obtain the address with GetProcAddress! (We have to patch KiXXX itself
			not the import thunk.)*/ 
		hNtdllModule = GetModuleHandle("ntdll.dll");
		targetProc = GetProcAddress(hNtdllModule,"KiUserExceptionDispatcher");
		if (!hNtdllModule || !targetProc || DetourTransactionBegin() != NO_ERROR) {}
		{
			kiUserExceptionDispatcher=targetProc;
			kiUserExceptionDispatcherHook=(FARPROC)hook;

			if (DetourUpdateThread(GetCurrentThread()) != NO_ERROR) {}
			else if (DetourAttach((PVOID *)&kiUserExceptionDispatcher,__hooked__KiUserExceptionDispatcher) != NO_ERROR) {}
			else
			{
				success = 1;
			}
			if (DetourTransactionCommit() != NO_ERROR) success = 0;
		}
		if (!success)
		{
			kiUserExceptionDispatcher=NULL;
			kiUserExceptionDispatcherHook=NULL;
		}
	}
	return success;
}
