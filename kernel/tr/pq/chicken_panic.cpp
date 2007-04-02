#include "chicken_panic.h"
#include "common/errdbg/error_codes.h"
#include "common/errdbg/exceptions.h"

extern "C" void chicken_panic_throw_exception(char* msg)
{
   throw USER_EXCEPTION2(SE5101, msg);  
}