/*
 * File:  boundaries.h
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BOUNDARIES_H
#define _BOUNDARIES_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr/xptr.h"

void storage_on_transaction_begin();
void storage_on_transaction_end();

#endif /* _BOUNDARIES_H */

