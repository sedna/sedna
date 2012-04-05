#include "common/errdbg/event_log.h"
#include "u/uprocess.h"
#include "u/u.h"
#include "common/base.h"

#include <iostream>
#include <string>

/* Attention! you need to make folder utest/data in your build directory to perform this test */

static UGlobalNamesRegistryItem globalNamesRegistry[] =
{
        /* {% GlobalNamesRegistry */ 
        {"SHMEM_EVENT_LOG",                                     NULL,1}, /* event logger */ 
        {"SEMAR_EVENT_LOG",                                     NULL,1}, /* event logger  */ 
        {NULL}
};

char SE_EVENT_LOG_SHARED_MEMORY_NAME__buf__[128];
char SE_EVENT_LOG_SEMAPHORES_NAME__buf__[128];

int main () {
  try {
    SEDNA_DATA = (char *) malloc(2048);
  
    std::string elog_location = uGetImageProcPath(SEDNA_DATA, __sys_call_error)/* + std::string("/test_log.log")*/;
    strcpy(SEDNA_DATA, elog_location.c_str());
    
    UInitGlobalNamesRegistry(globalNamesRegistry, NULL, 1000, 1400);
    
    global_name SE_EVENT_LOG_SHARED_MEMORY_NAME = 
                UCreateGlobalName("SHMEM_EVENT_LOG", 0, SE_EVENT_LOG_SHARED_MEMORY_NAME__buf__, 128);
    global_name SE_EVENT_LOG_SEMAPHORES_NAME = 
                UCreateGlobalName("SEMAR_EVENT_LOG", 0, SE_EVENT_LOG_SEMAPHORES_NAME__buf__, 128);
    
    
    /* Check that event log works */ 
    for (int log_level = 0; log_level < 5; log_level++) {
      if (event_logger_start_daemon(el_convert_log_level(log_level), SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME))
          throw std::string("Failed to initialize event log");
      
      elog(log_level, ("Hi, I'm log!"));
      
      event_logger_shutdown_daemon(SE_EVENT_LOG_SHARED_MEMORY_NAME);
    }
    
    /* Check that event log isn't stuck in infinite loop if it's down*/
    for (int log_level = 0; log_level < 5; log_level++) {
      if (event_logger_start_daemon(el_convert_log_level(log_level), SE_EVENT_LOG_SHARED_MEMORY_NAME, SE_EVENT_LOG_SEMAPHORES_NAME))
          throw std::string("Failed to initialize event log");
      event_logger_shutdown_daemon(SE_EVENT_LOG_SHARED_MEMORY_NAME);
      
      
      /* TODO: when syslog would be done we need to check it for this message */
      /* This message will never appear in event.log. If test hangs at this point or 
       * falls with stack overflow, it's an error.
       */
      elog(log_level, ("If you are in debug mode and you see this line as active, then I'm in infinite loop!"));
    }
    
  } catch (std::string a) { std::cout << a; }
  
  
  
  
  
  return 0;
}
