/*
 * File:  scheme_utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __SCHEME_UTILS_H
#define __SCHEME_UTILS_H

#include "common/xptr.h"
#include "tr/structures/nodes.h"

xptr sch_right_sibling(xptr& node);
xptr sch_left_sibling(xptr& node);
xptr sch_child_dm(xptr& node);
t_item sch_kind(xptr& node);
char* sch_name(xptr& node);

#endif
