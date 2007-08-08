#ifndef _SM_LLMGR_H
#define _SM_LLMGR_H

#include "sm/llmgr/llmgr_core.h"

class sm_llmgr : public llmgr_core
{
public:
  LONG_LSN recover_db_by_phys_records(/*const LONG_LSN& last_cp_lsn,*/ bool sync);
  void restorePh();
  void ll_log_checkpoint(WuEnumerateVersionsParams *params, WuVersionEntry *buf, size_t count, int isGarbage);
};

#endif