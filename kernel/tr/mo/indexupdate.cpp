/*
 * File:  indexupdate.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/mo/indexupdate.h"
#include "tr/idx/index_data.h"
#include "tr/crmutils/node_utils.h"
#include "tr/structures/schema.h"

#define CAT_FOR_EACH(T, list) for (cat_list<T>::item * i = list.first; i != NULL; i = i->next)

void indexDeleteNode_int(schema_node_cptr schema_node, xptr node)
{
    xptr key, object;
    CAT_FOR_EACH(index_ref, schema_node->index_list) {
        key = getNodeAncestorBySchemeCP(node, i->object.key);
        if (key != XNULL) { /* node's schema_node may differ from schema_node parameter (on text node insert, for example) */
            object = getNodeAncestorIndirectionByScheme(key, i->object.object);
            i->object.index->delete_from_index(key, object);
        }
    }
}

void indexAddNode_int(schema_node_cptr schema_node, xptr node)
{
    xptr key, object;
    CAT_FOR_EACH(index_ref, schema_node->index_list) {
        key = getNodeAncestorBySchemeCP(node, i->object.key);
        if (key != XNULL) { /* node's schema_node may differ from schema_node parameter (on text node delete, for example) */
            object = getNodeAncestorIndirectionByScheme(key, i->object.object);
            i->object.index->put_to_index(key, object);
        }
    }
}
