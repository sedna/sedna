/*
 * File:  nodemoutils.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/nodemoutils.h"
#include "tr/mo/exinfo.h"

using namespace internal;

void initNodeBlock(xptr block, shft dsc_size, schema_node_cptr schema_node)
{
    node_blk_hdr * hdr = (node_blk_hdr *) XADDR(block);
    memset(((char *) hdr) + sizeof(vmm_sm_blk_hdr), 0, sizeof(node_blk_hdr) - sizeof(vmm_sm_blk_hdr));

    hdr->dsc_size = dsc_size;
    hdr->snode = schema_node.ptr();
    hdr->node_type = schema_node->type;
    hdr->sz = (shft) sizeof(node_blk_hdr);


    if (hdr->node_type == text) { // Kinda optimization =)
//        nodeExtInit(schema_node, block);
    }

    clearNodeBlock(block);
}

void clearNodeBlock(xptr block)
{
    char * p = (char *) XADDR(block);
    node_blk_hdr * blk = (node_blk_hdr *) XADDR(block);

    memset(blk + 1, 0, PAGE_SIZE - sizeof(node_blk_hdr));

//    nodeExtClear(block);

    blk->free_first = blk->sz;
    blk->desc_first = 0;
    blk->desc_last = 0;
    blk->count = 0;
    blk->indir_count = 0;
    blk->free_first_indir = PAGE_SIZE-sizeof(xptr);

    shft descp = blk->free_first;
    shft desci = blk->free_first_indir;

    while (descp + 2*blk->dsc_size < desci - (shft) sizeof(xptr)) {
        *((shft *)(p + descp)) = descp + blk->dsc_size;
        *((shft *)(p + desci)) = desci - sizeof(xptr);

        descp += blk->dsc_size;
        desci -= sizeof(xptr);
    }

    *((shft *)(p + descp))=0;
    *((shft *)(p + desci))=0;
}
