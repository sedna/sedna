/*
 * File:  crmutils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CRMUTILS_H
#define _CRMUTILS_H

#include <map>

#include "common/sedna.h"

#include "common/xptr.h"
#include "tr/crmutils/exec_output.h"
#include "tr/structures/nodes.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPBase.h"
#include "tr/strings/strings.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif




extern se_stdlib_ostream crm_out;
typedef std::pair<int,int> stat_pair;

/* type of print */
enum t_print {
	xml,
	sxml
};

struct debug_info
{
    long schema_count;
	long schema_str_count;
	long block_count;
    long block_fill;
	long inner_block_count;
	__int64 inner_block_fill;
	float inner_fill_percentage;
	float fill_percentage;
	long str_blocks;
	__int64 node_count;
	__int64 ext_nid_count;
	long mdpth;//max depth
	long cdp;//current depth
	long freestrspace;//free space in string blocks
    debug_info() 
	{ 
		block_count=0;
		block_fill=0; 
		fill_percentage=0;
		inner_fill_percentage=0;
		schema_count=0;
		node_count=0;
		inner_block_count=0;
	    inner_block_fill=0;
		str_blocks=0;
		mdpth=0;
		cdp=0;
		ext_nid_count=0;
		schema_str_count=0;
		freestrspace=0;

	}
    
};

/* initialization of standard output*/
void init_output();
void print_tuple(const tuple &tup, se_ostream& crmout,bool ind,t_print ptype,bool is_first,dynamic_context *cxt);
void print_tuple(const tuple &tup, se_ostream& crmout,t_print ptype,dynamic_context *cxt);
void print_tuple_indent(const tuple &tup, se_ostream& crmout,t_print ptype,bool is_first,dynamic_context *cxt);
void print_node(xptr node, se_ostream& crmout,t_print ptype,dynamic_context *cxt);
void print_node_indent(xptr node, se_ostream& crmout,t_print ptype,dynamic_context *cxt);
void print_node_with_prefixes(xptr node, se_ostream& crmout, int indent);

/* prints information in block header */
void print_desc_block_hdr(node_blk_hdr* block, se_ostream& crmout);

/* prints information in element descriptor */
void print_element(e_dsc* node,int shift,shft size,schema_node* scm, se_ostream& crmout);

/* prints information in document descriptor */
void print_document(d_dsc* node,int shift,shft size,schema_node* scm, se_ostream& crmout);

/* prints information in text descriptor */
void print_text(t_dsc* node,int shift,  se_ostream& crmout, t_item xq_type);

/* prints information in attribute descriptor */
void print_attribute(a_dsc* node,int shift,  se_ostream& crmout);

/* prints information in  descriptor */
void print_descriptor(n_dsc* node,int shift, se_ostream& crmout);

/* prints information in  schema node */
void print_schema(schema_node* node, se_ostream& crmout);
/* prints descriptive schema  of stand-alone document*/
void print_descriptive_schema(const char * docname, se_ostream& crmout);

/* prints descriptive schema  of collection*/
void print_descriptive_schema_col(const char * colname, se_ostream& crmout);

// SXML analogues
void sxml_print_descriptive_schema(const char * docname, se_ostream& crmout);
void sxml_print_descriptive_schema_col(const char * colname, se_ostream& crmout);

/* prints the list of metadata features*/
void print_metadata(se_ostream& crmout);

/* prints the list of documents*/
void print_documents(se_ostream& crmout, bool ps=true);

/* prints the list of documents in the selected collection*/
void print_documents_in_collection(se_ostream& crmout, const char* collection);

/* prints the list of collections*/
void print_collections(se_ostream& crmout, bool ps=true);


/* returns type of  node */
char* convert_type(t_item type);


/* prints information in block */
void print_desc_block(xptr block, se_ostream& crmout);
void basicTest();
xptr loadfile(FILE* f, se_ostream &s, const char* uri,bool stripped,int& need_cp, bool print_progress);
xptr loadfile(FILE* f, se_ostream &s, const char* uri,const char * collection, bool stripped,int& need_cp, bool print_progress);
void print_text(xptr text, se_ostream& crmout,t_print ptype,t_item xq_type);
void print_text_block(xptr block, se_ostream& crmout);

//DEBUGUTILS
void getDebugInfo(schema_node* snode, debug_info* d_in);
void getSimpleDebugInfo(schema_node* snode, debug_info* d_in);
void checkTextNodeCorrectness(xptr node);
void checkChildReferenceValidity(xptr node);
#ifdef VMM_GATHER_STATISTICS
void printDebugInfo(schema_node* snode, se_ostream& crmout);
#endif
void printSimpleDebugInfo(schema_node* snode, se_ostream& crmout);
void getDebugInfo(schema_node* snode, xptr& node);
void printMFO (schema_node* node,std::map<schema_node*, std::pair<int,int> >  &mfo,int par_pref,int indent);

void isSchemaPCAllRight(schema_node* snode);
void testSaDoc(const char* docname);

/*
 * System tables utils
 */

enum document_type {
    DT_NON_SYSTEM,
    DT_DOCUMENTS,
    DT_INDEXES,
    DT_FTINDEXES,
    DT_TRIGGERS,
    DT_SCHEMA,
    DT_COLLECTIONS,
    DT_ERRORS,
    DT_VERSION,
    DT_MODULES,
    DT_DOCUMENT_,
    DT_COLLECTION_,
    DT_SCHEMA_
};


////////////////////////////////////////////////////////////////////////////////////////////////
/// The following methods return DT_NON_SYSTEM if given name is not one of the reserved.
document_type get_document_type(counted_ptr<db_entity> db_ent);
document_type get_document_type(const char* title, db_entity_type type);
////////////////////////////////////////////////////////////////////////////////////////////////


schema_node* get_system_doc(document_type type, const char* title);
void system_tables_on_kernel_statement_end();

//various output of xml document to string_buffer

#ifdef SE_ENABLE_FTSEARCH
void print_node_to_buffer(xptr node,op_str_buf& tbuf,ft_index_type type,pers_sset<ft_custom_cell,unsigned short> * custom_tree=NULL, const char *opentag="<", const char *closetag=">");
#endif
#endif

