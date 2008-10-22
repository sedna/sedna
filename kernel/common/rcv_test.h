#ifndef __RCV_TEST_H__
#define __RCV_TEST_H__

//#define RCV_TEST_CRASH

#ifdef RCV_TEST_CRASH
#define RECOVERY_CRASH rcvTestCrashPoint(__SE_FUNCTION__)
#else
#define RECOVERY_CRASH
#endif

// read test_rcv.xml and parses it to a table
void rcvReadTestCfg();

// check if function name has been considered in cfg and crashes tr with some probability
void rcvTestCrashPoint(const char *func_name);

#endif

