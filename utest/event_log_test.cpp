#define TEST_NAME "0002.eventLogTest.xml"

#include "common/sedna.h"
#include "u/uprocess.h"
#include "u/u.h"
#include "common/base.h"
#include "u/ugnames.h"
#include "gtest/gtest.h"
#include "common/globalobjects/globalnames.h"

#include <iostream>
#include <string>

std::string elog_location = "/tmp/";

TEST(eventLog, SimpleWorkTest) {
/* Check that event log works */ 
    for (int log_level = 1; log_level < 5; log_level++) {
      
      try {
        int res = 0;
        res = event_logger_start_daemon(SEDNA_DATA,
                                        el_convert_log_level(log_level),
                                        "SE_EVENT_LOG_SHARED_MEMORY_NAME", 
                                        "SE_EVENT_LOG_SEMAPHORES_NAME");
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
        
        res = event_logger_shutdown_daemon("SE_EVENT_LOG_SHARED_MEMORY_NAME");
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
      } catch (std::string a) { 
          std::cout << a; 
          ASSERT_FALSE(true);
      }
    }
}

TEST(eventLog, InfiniteLoopOnDownCheck) {
/* Check that event log isn't stuck in infinite loop if it's down*/
    for (int log_level = 0; log_level < 5; log_level++) {
      try {
        int res = 0;
        res = event_logger_start_daemon(SEDNA_DATA,
                                        el_convert_log_level(log_level), 
                                        "SE_EVENT_LOG_SHARED_MEMORY_NAME", 
                                        "SE_EVENT_LOG_SEMAPHORES_NAME");
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
        event_logger_shutdown_daemon("SE_EVENT_LOG_SHARED_MEMORY_NAME");
      
      
        /* TODO: when syslog would be done we need to check it for this message */
        /* This message will never appear in event.log. If test hangs at this point or 
        * falls with stack overflow, it's an error.
        */
        elog_long(log_level, "Hi, I'm long message in log!", "And you shouldn't see me too");
        elog(log_level, ("If you are in debug mode and you see this line as active, then I'm in infinite loop!"));
      } catch (std::string a) { 
          std::cout << a; 
          ASSERT_FALSE(true);
      }
    }

}

int main(int argc, char** argv) {
    if(argc == 1) {
        std::string cmd;
#ifdef WIN32
        cmd.append("xml:reports\\");
#else
        cmd.append("xml:reports/");
#endif   
//         cmd.append(TEST_NAME);
         ::testing::GTEST_FLAG(output) = cmd.c_str();
         ::testing::GTEST_FLAG(stack_trace_depth) = ::testing::kMaxStackTraceDepth;
    }
    ::testing::InitGoogleTest(&argc, argv);   
    
    char * SEDNA_DATA = (char *) malloc(2048);
    GlobalObjectsCollector collector(elog_location.c_str());
    uSetGlobalNameGeneratorBase(elog_location.c_str(), "0");
    strcpy(SEDNA_DATA, elog_location.c_str());

  
    return RUN_ALL_TESTS();
}
