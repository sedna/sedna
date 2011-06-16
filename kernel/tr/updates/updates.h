/*
 * File:  updates.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _UPDATES_H
#define _UPDATES_H

#include <map>

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/crmutils/crmbase.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/PPBase.h"

#include "tr/structures/schema.h"
#include "tr/structures/xmlns.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#include "tr/cat/catptr.h"
#endif

typedef  std::map<xmlns_ptr, xmlns_ptr> upd_ns_map;
//update physical operations
// INSERT TO operation
void insert_to(PPOpIn arg2, PPOpIn arg1);
void insert_following(PPOpIn arg2, PPOpIn arg1);
void insert_before(PPOpIn arg2, PPOpIn arg1) ;
// DELETE operation
void delete_deep(PPOpIn arg);
void delete_undeep(PPOpIn arg);
void replace(PPOpIn arg);

// Rename operation
void rename(PPOpIn arg, xsd::NCName name);
//update utilities
//void checkSwiizleTab(upd_ns_map*& updmap);
//insertion of the node to new parent

//inner operations
xptr copy_to_temp(xptr node);
xptr deep_copy_node(xptr left, xptr right, xptr parent, xptr node, upd_ns_map** nsupdmap, bool save_types, unsigned short depth=0);

/** copy_node_content
 * Copies content of the node to new node
 * Returns (!) indirection to the last inserted node
 */
xptr copy_node_content(xptr new_node_i, xptr node, xptr left_node_i, upd_ns_map** nsupdmap, bool save_types, unsigned short depth=0);

inline xptr deep_copy_node_i(xptr left_ind, xptr right_ind, xptr parent_ind, xptr node, upd_ns_map** nsupdmap, bool save_types,
        unsigned short depth = 0) {
    return deep_copy_node(indirectionDereferenceCP(left_ind), indirectionDereferenceCP(right_ind), indirectionDereferenceCP(parent_ind),
            node, nsupdmap, save_types, depth);
}

inline xptr deep_copy_node_ii(xptr left_ind, xptr right_ind, xptr parent_ind, xptr node, upd_ns_map** nsupdmap, bool save_types,
        unsigned short depth = 0) {
    return getIndirectionSafeCP(deep_copy_node(indirectionDereferenceCP(left_ind), indirectionDereferenceCP(right_ind), indirectionDereferenceCP(parent_ind),
            node, nsupdmap, save_types, depth));
}

inline xptr deep_copy_node_t(xptr left_ind, xptr right_ind, xptr parent_ind, xptr node) {
    upd_ns_map* ins_swiz = NULL;

    xptr result = deep_copy_node(indirectionDereferenceCP(left_ind), indirectionDereferenceCP(right_ind),
            indirectionDereferenceCP(parent_ind), node, &ins_swiz, true);

    if (ins_swiz != NULL) {
        delete ins_swiz;
    }

    return result;
}

/** Internal function, also used in constructors
  * It generates a new namespace prefix, if given is already bound to another URI
  * in given parent node. The case of default namespace is specially handled.
  * new_ns MUST NOT be NULL_XMLNS
  * Returns new namespace if one is swizzeled, and NULL_XMLNS if not.
  */
xmlns_ptr swizzle_namespace(xptr node, xmlns_ptr new_ns);

//node tests

bool inline is_node_persistent (xptr node)
{
    return IS_DATA_BLOCK(node);
}
//checks whether the node is  element
bool inline is_node_element (xptr node)
{
    return (getNodeType(node)==element);
}
//checks whether the node is  text
bool inline is_node_text (xptr node)
{
    return (getNodeType(node)==text);
}
//checks whether the  nodes are from different storage
bool inline is_different_storage (xptr node1,xptr node2)
{
    return (IS_DATA_BLOCK(node1)&&!IS_DATA_BLOCK(node2) );
}
//checks whether the node is  element
bool inline is_node_document (xptr node)
{
    return (getNodeType(node)==document);
}

//checks whether the node is  element
bool inline is_node_attribute (xptr node)
{
    return (getNodeType(node)==attribute);
}


//checks whether the node is  attribute


bool inline is_node_xml_namespace (xptr node)
{
 return (getNodeType(node)==xml_namespace);
}
t_item inline get_node_type (xptr node)
{
 return (getNodeType(node));
}



#ifdef SE_ENABLE_FTSEARCH
void clear_ft_sequences();
void execute_modifications();
void update_insert_sequence(xptr node,ft_index_cell_cptr icell);
void update_update_sequence(xptr node,ft_index_cell_cptr icell);
void update_delete_sequence(xptr node,ft_index_cell_cptr icell);

void update_insert_sequence(xptr node,schema_node_cptr icell);
void update_update_sequence(xptr node,schema_node_cptr icell);
void update_delete_sequence(xptr node,schema_node_cptr icell);

void init_ft_sequences (const xptr& left, const xptr& right, const xptr& parent);
#endif

/* Bulk load */

xptr loadfile(FILE* f, se_ostream &ostr, const char* uri,
              bool stripped,
              bool print_progress);

xptr loadfile(FILE* f, se_ostream &ostr, const char* uri,
              const char * collection, bool stripped,
              bool print_progress);

#endif

