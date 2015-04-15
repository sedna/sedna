/*
 * File:  sedna.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SEDNA_H
#define _SEDNA_H

#include "u/u.h"
#include "u/uni.h"

#include <stdint.h>

#include "common/errdbg/event_log.h"
//#include "common/errdbg/error_codes.h" //TODO: should it really be here? If yes, need to fix build sequence
#include "common/errdbg/errors.h"

#ifdef __cplusplus
#include <cstddef>
#include "common/errdbg/exceptions.h"
#endif

#define RECOVERY_CRASH { }

#endif /*_SEDNA_H */
