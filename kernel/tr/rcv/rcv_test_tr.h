#ifndef __RCV_TEST_TR_H__
#define __RCV_TEST_TR_H__

#ifdef RCV_TEST_CRASH
#define TEST_AFTER_RCV
#endif

// checks all documents and collections for consistency (by employing checkTreeConsistency from PPTest)
void test_db_after_rcv();

#endif
