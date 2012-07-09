#include "common/sedna.h"
#include "u/uprocess.h"
#include "u/u.h"
#include "common/base.h"
#include "u/ugnames.h"

#include <iostream>
#include <string>

/* Attention! you need to make folder utest/data in your build directory to perform this test */

static UGlobalNamesRegistryItem globalNamesRegistry[] =
{
  /* {% GlobalNamesRegistry */ 
  {"SHMEM_EVENT_LOG", NULL,1}, /* event logger */ 
  {"SEMAR_EVENT_LOG", NULL,1}, /* event logger  */ 
  {NULL}
};

char SE_EVENT_LOG_SHARED_MEMORY_NAME__buf__[128];
char SE_EVENT_LOG_SEMAPHORES_NAME__buf__[128];

int main () {
    char * SEDNA_DATA = (char *) malloc(2048);
  
    std::string elog_location = uGetImageProcPath(SEDNA_DATA, __sys_call_error);
    strcpy(SEDNA_DATA, elog_location.c_str());
    
    UInitGlobalNamesRegistry(globalNamesRegistry, NULL, NULL, 1000, 1400);
    
    global_name SE_EVENT_LOG_SHARED_MEMORY_NAME = 
                UCreateGlobalName("SHMEM_EVENT_LOG", 
                                  0, 
                                  SE_EVENT_LOG_SHARED_MEMORY_NAME__buf__, 
                                  128);
                
    global_name SE_EVENT_LOG_SEMAPHORES_NAME = 
                UCreateGlobalName("SEMAR_EVENT_LOG", 
                                  0, 
                                  SE_EVENT_LOG_SEMAPHORES_NAME__buf__, 
                                  128);
    
    
    /* Check that event log works */ 
    for (int log_level = 1; log_level < 5; log_level++) {
      
      try {
        int res = 0;
        res = event_logger_start_daemon(el_convert_log_level(log_level), 
                                        SE_EVENT_LOG_SHARED_MEMORY_NAME, 
                                        SE_EVENT_LOG_SEMAPHORES_NAME);
        switch (res) {
          case 0: break;
          case 1: throw std::string("Failed to initialize event log: can not create shared memory\n");
                  break;
          case 2: throw std::string("Failed to initialize event log: can not attach to shared memory\n");
                  break;
          case 3: throw std::string("Failed to initialize event log: can not create semaphore array\n");
                  break;                
          case 4: throw std::string("Failed to initialize event log: can not create thread\n");
                  break;
          default: break;
        }

        
        elog(log_level, ("Hi, I'm log and I'm started successfully!"));
        elog_long(log_level, "Hi, I'm long message in log!", " Not really very long but I'm needed just for test"); 
        
        res = event_logger_shutdown_daemon(SE_EVENT_LOG_SHARED_MEMORY_NAME);
        switch (res) {
          case 0: break;
          case 1: throw std::string("Failed to shutdown event log: can not join thread\n");
                  break;
          case 2: throw std::string("Failed to shutdown event log: can not close thread handle\n");
                  break;
          case 3: throw std::string("Failed to shutdown event log: can not release semaphore array\n");
                  break;                
          case 4: throw std::string("Failed to shutdown event log: can not detach shared memory\n");
                  break;
          case 5: throw std::string("Failed to shutdown event log: can not release shared memory\n");
                  break;
          default: break;
        }
      } catch (std::string a) { std::cout << a; }
    }
    

    /* Check that event log isn't stuck in infinite loop if it's down*/
    for (int log_level = 0; log_level < 5; log_level++) {
      try {
        int res = 0;
        res = event_logger_start_daemon(el_convert_log_level(log_level), 
                                        SE_EVENT_LOG_SHARED_MEMORY_NAME, 
                                        SE_EVENT_LOG_SEMAPHORES_NAME);
        switch (res) {
          case 0: break;
          case 1: throw std::string("Failed to initialize event log: can not create shared memory\n");
                  break;
          case 2: throw std::string("Failed to initialize event log: can not attach to shared memory\n");
                  break;
          case 3: throw std::string("Failed to initialize event log: can not create semaphore array\n");
                  break;                
          case 4: throw std::string("Failed to initialize event log: can not create thread\n");
                  break;
          default: break;
        }
        event_logger_shutdown_daemon(SE_EVENT_LOG_SHARED_MEMORY_NAME);
      
      
        /* TODO: when syslog would be done we need to check it for this message */
        /* This message will never appear in event.log. If test hangs at this point or 
        * falls with stack overflow, it's an error.
        */
        elog_long(log_level, "Hi, I'm long message in log!", "And you shouldn't see me too");
        elog(log_level, ("If you are in debug mode and you see this line as active, then I'm in infinite loop!"));
      } catch (std::string a) { std::cout << a; }
    }
  
    return 0;
}
