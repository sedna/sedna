/*
 * File:  indexupdate.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDEXUPDATE_H
#define _INDEXUPDATE_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr/xptr.h"
#include "tr/cat/catptr.h"
#include "tr/structures/schema.h"

void indexDeleteNode_int(schema_node_cptr schema_node, xptr node);
void indexAddNode_int(schema_node_cptr schema_node, xptr node);

inline void indexDeleteNode(schema_node_cptr schema_node, xptr node)
{
    if (!schema_node->index_list->empty()) {
        indexDeleteNode_int(schema_node, node);
    }
}

inline void indexAddNode(schema_node_cptr schema_node, xptr node)
{
    if (!schema_node->index_list->empty()) {
        indexAddNode_int(schema_node, node);
    }
}

#endif /* _INDEXUPDATE_H */
