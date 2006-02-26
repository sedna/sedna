/*
 * File:  merge.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _MERGE_H
#define _MERGE_H

#include "xptr.h"
#include "node_utils.h"

int doc_order_merge_cmp(const void *e1, const void *e2);

class RelChildAxisMerge
{
private:
    xptr* merged_seq_arr;
    int size;

public:
    RelChildAxisMerge() : merged_seq_arr(NULL), size(0) {}
    ~RelChildAxisMerge() { delete [] merged_seq_arr; merged_seq_arr = NULL; }

    xptr init(const xptr& parent, const char* uri, const char* name, t_item type, comp_schema cfun);
    xptr next(const xptr &p);
};


class RelAttrAxisMerge
{
private:
    xptr* merged_seq_arr;
    int size;
    int pos;

public:
    RelAttrAxisMerge() : merged_seq_arr(NULL), size(0) {}
    ~RelAttrAxisMerge() { delete [] merged_seq_arr; merged_seq_arr = NULL; }

    xptr init(const xptr& parent, const char* uri, const char* name, t_item type, comp_schema cfun);
    xptr next();
};


#endif

