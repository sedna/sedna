#ifndef _SM_TYPE_H_
#define _SM_TYPE_H_

#include "common/sedna.h"
#include "common/base.h"

#include <stdint.h>

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

inline
int64_t MBS2PAGES(int64_t s) {
    return (s * 0x100000ULL / PAGE_SIZE);
}

inline
int64_t PAGES2MBS(int64_t s) {
    return (s * PAGE_SIZE / 0x100000ULL);
}

#endif /* _SM_TYPE_H_ */
