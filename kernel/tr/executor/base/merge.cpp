/*
 * File:  merge.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "merge.h"
#include "node_utils.h"
#include "utils.h"

int doc_order_merge_cmp(const void *e1, const void *e2)
{
    xptr *el1 = (xptr*)e1;
    xptr *el2 = (xptr*)e2;

    if (*el1 == NULL && *el2 == NULL) return 0;
    if (*el1 == NULL) return 1;
    if (*el2 == NULL) return -1;

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

    xptr p;
    size = getChildrenXptr(parent, uri, name, type, cfun, p, merged_seq_arr);
    if (size > 1)
    {
        qsort(merged_seq_arr, size, sizeof(xptr), doc_order_merge_cmp);

        p = merged_seq_arr[0];
        merged_seq_arr[0] = getNextSiblingOfSameSortXptr(p);
    }

    return p;
}

xptr RelChildAxisMerge::next(const xptr &p)
{
    if (merged_seq_arr)
    {
		xptr p;
        elim_disturb(merged_seq_arr, size, sizeof(xptr), doc_order_merge_cmp);
		p = merged_seq_arr[0];
		if (p != NULL)
			merged_seq_arr[0] = getNextSiblingOfSameSortXptr(p);
        return p;
    }
    else return getNextSiblingOfSameSortXptr(p);
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

    xptr p;
    size = getChildrenXptr(parent, uri, name, type, cfun, p, merged_seq_arr);
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
