/*
 * File:  micro.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _MICRO_H
#define _MICRO_H

#include "common/sedna.h"
#include "common/xptr.h"
#include "tr/structures/nodes.h"
#include "tr/crmutils/node_utils.h"
#include "tr/structures/schema.h"
#include <string.h>
#include "common/persistent_db_data.h"
#include "tr/vmm/vmm.h"

//#define FASTDELETE

//#define _MYDEBUG
//#define _MYDEBUG1
/* type of schema node (item) */
enum text_type {
	text_mem,
	text_doc,
	text_estr
};
xptr insert_element(xptr left_sib, xptr right_sib, xptr parent,const char* name, xmlscm_type type,xml_ns* ns);
inline xptr insert_element(xptr left_sib, xptr right_sib, xptr parent,const char* name, xmlscm_type type,const char* uri, const char* prefix)
{
	if (uri!=NULL || prefix!=NULL)
	{
		xml_ns* ns=(xml_ns*)entry_point->nslist->find(uri,prefix);
		if (ns==NULL) 
		{
			ns=xml_ns::init(uri,prefix,true);
			entry_point->nslist->put(ns);
		}
		return insert_element(left_sib,right_sib,parent,name,type,ns);
	}
	else
		return insert_element(left_sib,right_sib,parent,name,type,NULL);

	
}

xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent,const char* name, xmlscm_type type,const  char* value, int data_size,xml_ns* ns);
inline xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent,const char* name, xmlscm_type type,const  char* value, int data_size,const char* uri, const char* prefix)
{
	if (uri!=NULL || prefix!=NULL)
	{
		xml_ns* ns=(xml_ns*)entry_point->nslist->find(uri,prefix);
		if (ns==NULL) 
		{
			ns=xml_ns::init(uri,prefix,true);
			entry_point->nslist->put(ns);
		}
		return insert_attribute(left_sib, right_sib, parent,name, type,value, data_size,ns);
	}
	return insert_attribute(left_sib, right_sib, parent,name, type,value, data_size,NULL);
}

xptr insert_text(xptr left_sib, xptr right_sib, xptr parent,const void* value, unsigned int size,text_type ttype=text_mem);
xptr insert_comment(xptr left_sib, xptr right_sib, xptr parent,const char* value, int size);
xptr insert_cdata(xptr left_sib, xptr right_sib, xptr parent,const char* value, int size);
xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent,const char* target, int tsize,const char* data, int dsize);
xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent,xml_ns* ns);
inline xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent,const char* uri, const char* prefix)
{
	xml_ns* ns=(xml_ns*)entry_point->nslist->find(uri,prefix);
	if (ns==NULL) 
	{
		ns=xml_ns::init(uri,prefix,true);
		entry_point->nslist->put(ns);
	}
	return insert_namespace(left_sib,right_sib,parent,ns);
}
void delete_node(xptr node);
void delete_replaced_node(xptr delete_node, xptr insert_node);
void delete_doc_node(xptr node);




/* Update utils*/
void makeNewBlockConsistentAfterFilling(xptr block, xptr node,shft shift_size);

/*node deletion*/
void delete_inner_nodes(n_dsc* node);

void deleteBlock(node_blk_hdr * block);

void makeBlockConsistentAfterCuttingTheEnd(node_blk_hdr *block,n_dsc* node,shft counter);

xptr createBlockNextToTheCurrentBlock (node_blk_hdr * block, const xptr & undo_hint = XNULL);

void shiftNodeToTheNewBlock(n_dsc* source,xptr dest,shft size,node_blk_hdr * block);

void updateChildPointer (n_dsc* parent,xptr old_xptr,xptr dest);

xptr insertBetween ( xptr left_sib, xptr right_sib, n_dsc* new_node);

xptr splitBlockIfFullAndCheckWhereTheNodeIs(xptr nodex);
int splitBlockIfFullAfterRightInsert(xptr& nodex);
int splitBlockIfFullAfterLeftInsert(xptr& nodex);

/* insert new node after the namesake element with the stated left-right siblings and parent*/
xptr addNewNodeOfSameSortAfter(xptr namesake, xptr left_sib,xptr right_sib, xptr parent, xptr par_indir, xmlscm_type type,t_item node_typ);

/* insert new node before the namesake element with the stated left-right siblings and parent*/
xptr addNewNodeOfSameSortBefore(xptr namesake, xptr left_sib,xptr right_sib, xptr parent, xptr par_indir, xmlscm_type type,t_item node_typ);

/* creates first block with the descriptor fo the given schema node*/
xptr createNewBlock(schema_node* scm,bool persistent);

/* inserts the first node descriptor of the current type*/
xptr addNewNodeFirstInRow(xptr newblock, xptr left_sib, xptr right_sib, xptr parent,
							 xptr par_indir ,  xmlscm_type type, t_item node_typ);
/*splits the block into two parts and appends childs by scheme into descriptor of one of the parts*/
void addChildsBySchemeSplittingBlock(xptr parent,const  char* name,t_item type, xptr child,xml_ns* ns);
/* index support */
void update_idx_add (xptr node);
void update_idx_add_txt (xptr node);
void update_idx_add (xptr node,const char* value, int size);
void update_idx_add_txt (xptr node,const char* value, int size);
void update_idx_delete (xptr node);
void update_idx_delete_text (xptr node);
void update_idx_delete_text (xptr node,const char* value, int size);
void update_idx_delete_text (schema_node* scm,xptr node,const char* value, int size);



/*
inline xptr removeIndirection(xptr indir)
{
	if (indir!=XNULL)
	{
		CHECKP(indir);
		return *((xptr*)XADDR(indir));
	}
	return XNULL;
}
*/

inline void copyDescriptor(n_dsc* source,xptr dest,  shft size)
{
	char *z = se_new char[size];
	memcpy(z,source,size);
	CHECKP(dest);
	VMM_SIGNAL_MODIFICATION(dest);
	memcpy(XADDR(dest),z,size);
	delete [] z;
}


/* all defines below represent unrealized functions*/
inline  void createNID(xptr left, xptr right, xptr parent,xptr result) 
{
	//return;
	if (left!=XNULL)
	{
		if (right!=XNULL) 
			 nid_create_between(left,right,result);
		else
			 nid_create_right(left,parent,result);
	}
	else
	if (right!=XNULL )
		 nid_create_left(right,parent,result);
	else
		 nid_create_child(parent,result);
}



/* clears references to child nodes*/
void inline clear_references(node_blk_hdr* block,n_dsc* node)
{
//    int chcnt=COUNTREFERENCES(block,size_of_node(block));

//	xptr* childx=(xptr*)((char*)node+size_of_node(block));

    memset((char*)node+size_of_node(block), 0, (block->dsc_size - size_of_node(block)));
/*
	for (int i=0;i<chcnt;i++)
	{
		RECOVERY_CRASH;
		*childx=XNULL;
		childx+=1;
	}
*/
}
//UNREALIZED

/*deletes text value from the database*/
void deleteTextValue(xptr node);

/*inserts text value into the database*/
void addTextValue(xptr node,const void* text, unsigned int size,text_type ttype=text_mem);


/*appends currently existing text value */
void appendTextValue(xptr node,const void* text, unsigned int size,text_type ttype);

/*appends currently existing text value. New  text is inserted from the first position */
void insertTextValue(xptr node,const void* text, unsigned int size,text_type ttype);
void delete_text_head(xptr node, int size);
void delete_text_tail(xptr node, int size);

/*redo operation*/
//void redoBlockCreation(xptr block,xptr left_n,xptr right_n,schema_node* sn, int desc_size);

/*decrement count*/
void decrement_count(node_blk_hdr* pr_blk,shft count=1);
/* increment count*/
void increment_count(node_blk_hdr* pr_blk,shft count=1);


#endif
