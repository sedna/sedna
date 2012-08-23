#ifndef _SM_TYPE_H_
#define _SM_TYPE_H_

#include "common/sedna.h"
#include "common/base.h"

#include "common/errdbg/d_printf.h"

#include "common/llcommon/lfsGlobals.h"
#include "common/structures/config_data.h"
#include "common/xptr/sm_vmm_data.h"

#include "common/ssmmsg/SSMMsg.h"
#include "common/llcommon/llMain.h"

#include "common/socketutils/socketutils.h"
#include "common/protocol/int_sp.h"

#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/blk_mngmt.h"

#include "sm/sm_globals.h"
#include "sm/sm_functions.h"

#define MBS2PAGES(s)   ((int64_t) (s) * (int64_t)0x100000 / (int64_t) PAGE_SIZE)
#define PAGES2MBS(s)   ((int64_t) (s) * (int64_t)PAGE_SIZE / (int64_t) 0x100000)

#endif /* _SM_TYPE_H_ */
