/*
 * File:  exinfo.h
 *
 * This file provides interfaces for block management operations (create, destroy, etc.)
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _EXINFO_H
#define _EXINFO_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/structures/schema.h"

void nodeExtInit(schema_node_cptr schema_node, xptr block);
void nodeExtClear(xptr block);

// Dummy for CData preservation

bool nodeExtGetCDFlag(xptr node);
bool nodeExtSetCDFlag(xptr node, bool flag = true);

#endif /* _EXINFO_H */
