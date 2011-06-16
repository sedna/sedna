/*
 * File:  exinfo.cpp
 *
 * This file provides interfaces for block management operations (create, destroy, etc.)
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/exinfo.h"

#include "tr/structures/nodeblocks.h"

using namespace internal;

void nodeExtInit(schema_node_cptr schema_node, xptr block) {
    if (schema_node->type != text) {
        return;
    }

    std::string a;
    a.substr();

    node_blk_hdr * header = getBlockHeaderCP(block);

    if (schema_node->type == text) {
        WRITEP(block);

        /* This approach may loose some space, however not very much (maybe for 1 node)
           TODO: more accurate space evaluation */

        header->sz = sizeof(node_blk_hdr); // important !
        shft cDFlagSpace = (getBlockCapacity(header) - 1) / 8 + 1;
        header->sz = sizeof(node_blk_hdr) + cDFlagSpace;
    }
}

void nodeExtClear(xptr block) {
    node_blk_hdr * header = getBlockHeaderCP(block);
    schema_node_cptr schema_node = header->snode;

    if (schema_node->type != text) {
        return;
    }

    if (schema_node->type == text) {
        WRITEP(block);

        shft cDFlagSpace = (getBlockCapacity(header) - 1) / 8 + 1;

        memset(header->additional_data, 0, cDFlagSpace);
    }
}

bool nodeExtGetCDFlag(xptr node) {

}

bool nodeExtSetCDFlag(xptr node, bool flag = true) {

}
