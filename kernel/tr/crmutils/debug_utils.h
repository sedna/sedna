/*
 * File:  debug_utils.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _DEBUG_UTILS_H
#define _DEBUG_UTILS_H

#include "tr/crmutils/crmbase.h"
#include "tr/cat/catptr.h"

void printSimpleDebugInfo(schema_node_cptr snode, se_ostream& crmout);
void getDebugInfo(schema_node_cptr snode, xptr& node);

#endif /* _DEBUG_UTILS_H */
