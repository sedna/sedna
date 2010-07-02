/*
 * File:  merge.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/base/merge.h"
#include "tr/nid/numb_scheme.h"
#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeutils.h"
#include "common/utils.h"

int doc_order_merge_cmp(const void *e1, const void *e2)
{
    xptr *el1 = (xptr*)e1;
    xptr *el2 = (xptr*)e2;

    if (*el1 == XNULL && *el2 == XNULL) return 0;
    if (*el1 == XNULL) return 1;
    if (*el2 == XNULL) return -1;

    return nid_cmp(*el1, *el2);
}


xptr RelChildAxisMerge::init(const xptr& parent, const char* uri, const char* name, t_item type, comp_schema cfun)
{
    if (merged_seq_arr)
    {
        delete merged_seq_arr;
        merged_seq_arr = NULL;
        size = 0;
    }

    xptr p = XNULL;
    size = collectChildren(parent, uri, name, type, cfun, p, merged_seq_arr);
    if (size > 1)
    {
        qsort(merged_seq_arr, size, sizeof(xptr), doc_order_merge_cmp);

        p = merged_seq_arr[0];
        merged_seq_arr[0] = getRightSiblingOfSameSort(p);
    }

    return p;
}

xptr RelChildAxisMerge::next(const xptr &p)
{
    if (merged_seq_arr)
    {
		xptr p = XNULL;
        elim_disturb(merged_seq_arr, size, sizeof(xptr), doc_order_merge_cmp);
		p = merged_seq_arr[0];
		if (p != XNULL)
			merged_seq_arr[0] = getRightSiblingOfSameSort(p);
        return p;
    }
    else return getRightSiblingOfSameSort(p);
}


xptr RelAttrAxisMerge::init(const xptr& parent, const char* uri, const char* name, t_item type, comp_schema cfun)
{
    if (merged_seq_arr)
    {
        delete merged_seq_arr;
        merged_seq_arr = NULL;
        size = 0;
        pos = 0;
    }

    xptr p = XNULL;
    size = collectChildren(parent, uri, name, type, cfun, p, merged_seq_arr);
    if (size > 1)
    {
        qsort(merged_seq_arr, size, sizeof(xptr), doc_order_merge_cmp);

        p = merged_seq_arr[0];
    }
    pos = 1;

    return p;
}

xptr RelAttrAxisMerge::next()
{
    if (pos < size) return merged_seq_arr[pos++];
    else return XNULL;
}
