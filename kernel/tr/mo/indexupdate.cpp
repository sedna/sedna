/*
 * File:  indexupdate.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/indexupdate.h"
#include "tr/idx/index_data.h"
#include "tr/crmutils/node_utils.h"
#include "tr/structures/schema.h"

#define CAT_FOR_EACH(T, list) for (cat_list<T>::item * i = list.first; i != NULL; i = i->next)

void update_idx_add(xptr node)
{
    CAT_FOR_EACH(index_ref, getBlockHeaderCP(node)->snode->index_list) {
        i->object.index->put_to_index(node, i->object.object);
    }
}

void update_idx_add(xptr node, const char* value, strsize_t size)
{
    CAT_FOR_EACH(index_ref, getBlockHeaderCP(node)->snode->index_list) {
        i->object.index->put_to_index(node, value, (size_t) size, i->object.object);
    }
}

void update_idx_add_text(xptr node) {
    if (getBlockHeaderCP(node)->snode->parent->index_list.empty()) return;
    
    update_idx_add(node);
    update_idx_add(getParentCP(node));
}


void update_idx_delete (xptr node)
{
    CAT_FOR_EACH(index_ref, getBlockHeaderCP(node)->snode->index_list) {
        i->object.index->delete_from_index(node, i->object.object);
    }
}

void update_idx_delete_text(xptr node) {
    if (getBlockHeaderCP(node)->snode->parent->index_list.empty()) return;

    update_idx_delete(node);
    update_idx_delete(getParentCP(node));
}
