/*
 * File:  updates.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _UPDATES_H
#define _UPDATES_H
#include "xptr.h"
#include "vmm.h"
#include "nodes.h"
#include "schema.h"
#include "PPBase.h"
#include <map>
#ifdef SE_ENABLE_FTSEARCH
#include "ft_index_data.h"
#endif

typedef  std::map< xml_ns*, xml_ns*> upd_ns_map;
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
void rename(PPOpIn arg,const char* name);
//update utilities
//void checkSwiizleTab(upd_ns_map*& updmap);
//insertion of the node to new parent
xptr deep_node_copy(xptr left_ind, xptr right_ind, xptr parent_ind, xptr node);

//inner operations
xptr copy_to_temp(xptr node);
void clear_temp();
xptr deep_pers_copy(xptr left, xptr right, xptr parent, xptr node,bool save_types, unsigned short depth=0);
void copy_content(xptr newnode,xptr node,xptr left);
xptr deep_temp_copy(xptr left, xptr right, xptr parent, xptr node,upd_ns_map*& updmap, unsigned short depth=0);
//node tests

bool inline is_node_persistent (xptr node)
{
	return IS_DATA_BLOCK(node);
}
//checks whether the node is  element
bool inline is_node_element (xptr node)
{
	return (GETSCHEMENODEX(node)->type==element);
}
//checks whether the node is  text
bool inline is_node_text (xptr node)
{
	return (GETSCHEMENODEX(node)->type==text);
}
//checks whether the  nodes are from different storage
bool inline is_different_storage (xptr node1,xptr node2)
{
	return (IS_DATA_BLOCK(node1)&&!IS_DATA_BLOCK(node2) );
}
//checks whether the node is  element
bool inline is_node_document (xptr node)
{
	return (GETSCHEMENODEX(node)->type==document);
}


//checks whether the node is  attribute
bool inline is_node_attribute (xptr node)
{
 return (GETSCHEMENODEX(node)->type==attribute);
}
bool inline is_node_child (xptr node)
{
	return (GETSCHEMENODEX(node)->type!=attribute &&GETSCHEMENODEX(node)->type!=xml_namespace);
}
bool inline is_node_xml_namespace (xptr node)
{
 return (GETSCHEMENODEX(node)->type==xml_namespace);
}
t_item inline get_node_type (xptr node)
{
 return (GETSCHEMENODEX(node)->type);
}


//checks whether the right sibling of the node is  attribute
bool inline is_next_node_attribute (xptr node)
{
	node=GETRIGHTPOINTER(node);
	if (node!=XNULL)
	{
		CHECKP(node);
		return is_node_attribute (node);
	}
	return false;
}
#ifdef SE_ENABLE_FTSEARCH
void clear_ft_sequences();
void execute_modifications();
void update_insert_sequence(xptr node,ft_index_cell* icell);
void update_update_sequence(xptr node,ft_index_cell* icell);
void update_delete_sequence(xptr node,ft_index_cell* icell);
void update_update_sequence(xptr node,schema_ft_ind_cell* icell);
void update_insert_sequence(xptr node,schema_ft_ind_cell* icell);
void update_delete_sequence(xptr node,schema_ft_ind_cell* icell);
void init_ft_sequences (xptr& left, xptr& right, xptr& parent);
#endif

#endif

