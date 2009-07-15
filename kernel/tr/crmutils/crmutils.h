/*
 * File:  crmutils.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _CRMUTILS_H
#define _CRMUTILS_H

#include <map>

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/crmutils/exec_output.h"
#include "tr/crmutils/crmbase.h"
#include "tr/structures/nodes.h"
#include "tr/strings/strings.h"
#include "tr/cat/catptr.h"

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif

class dynamic_context;

/* predefined debug&error output stream */
extern se_stdlib_ostream crm_dbg;

/* some statistics counters */
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
    long mdpth;           //max depth
    long cdp;             //current depth
    long freestrspace;    //free space in string blocks

    debug_info() : schema_count(0), 
                   schema_str_count(0),
 	               block_count(0),
                   block_fill(0),
                   inner_block_count(0),
                   inner_block_fill(0),
                   inner_fill_percentage(0),
                   fill_percentage(0),
                   str_blocks(0),
                   node_count(0),
                   ext_nid_count(0),
                   mdpth(0),
                   cdp(0),
                   freestrspace(0) {}
};

////////////////////////////////////////////////////////////////////////////////
/// Print utils 
////////////////////////////////////////////////////////////////////////////////

void 
print_tuple           (const tuple &tup,     /* tuple to print */
                       se_ostream& crmout,   /* output strem to print into */
                       dynamic_context *cxt, /* context to get namespaces */
                       t_print ptype,        /* xml, sxml, etc ... */
                       bool is_first,        /* is item first in result */
                       bool ind);            /* server indents result items*/


#ifdef SE_ENABLE_FTSEARCH
void print_node_to_buffer(xptr node,
                          op_str_buf& tbuf,
                          ft_index_type type, 
                          ft_custom_tree_t * custom_tree=NULL, 
                          const char *opentag="<", 
                          const char *closetag=">");
#endif

////////////////////////////////////////////////////////////////////////////////
/// Load utils 
////////////////////////////////////////////////////////////////////////////////

xptr loadfile(FILE* f, se_ostream &ostr, const char* uri, 
              bool stripped, int& need_cp, 
              bool print_progress);

xptr loadfile(FILE* f, se_ostream &ostr, const char* uri, 
              const char * collection, bool stripped, int& need_cp, 
              bool print_progress);

////////////////////////////////////////////////////////////////////////////////
/// Debug utils 
////////////////////////////////////////////////////////////////////////////////

/* prints information in block */
void print_desc_block(xptr block, se_ostream& crmout);
/* prints information in block header */
void print_desc_block_hdr(node_blk_hdr* block, se_ostream& crmout);

/* prints information in element descriptor */
void print_element(e_dsc* node,
                   int shift,
                   shft size,
                   schema_node_cptr scm, 
                   se_ostream& crmout);

/* prints information in document descriptor */
void print_document(d_dsc* node,
                    int shift,
                    shft size,
                    schema_node_cptr scm, 
                    se_ostream& crmout);

/* prints information in text descriptor */
void print_text(t_dsc* node,int shift,  se_ostream& crmout, t_item xq_type);
/* prints information in attribute descriptor */
void print_attribute(a_dsc* node,int shift,  se_ostream& crmout);
/* prints information in  descriptor */
void print_descriptor(n_dsc* node,int shift, se_ostream& crmout);
/* prints information in  schema node */
void print_schema(schema_node_cptr node, se_ostream& crmout);

void getDebugInfo(schema_node_cptr snode, debug_info* d_in);
void getSimpleDebugInfo(schema_node_cptr snode, debug_info* d_in);
void checkTextNodeCorrectness(xptr node);
void checkChildReferenceValidity(xptr node);

#ifdef VMM_GATHER_STATISTICS
void printDebugInfo(schema_node_cptr snode, se_ostream& crmout);
#endif

void printSimpleDebugInfo(schema_node_cptr snode, se_ostream& crmout);
void getDebugInfo(schema_node_cptr snode, xptr& node);

void printMFO (schema_node_cptr node, 
               std::map<schema_node_xptr, std::pair<int,int> >  &mfo,
               int par_pref,
               int indent);

void isSchemaPCAllRight(schema_node_cptr snode);
void testSaDoc(const char* docname);


////////////////////////////////////////////////////////////////////////////////
/// Legacy metadata printings
////////////////////////////////////////////////////////////////////////////////

/* prints descriptive schema  of stand-alone document*/
void print_descriptive_schema(const char * docname, se_ostream& crmout);
/* prints descriptive schema  of collection*/
void print_descriptive_schema_col(const char * colname, se_ostream& crmout);
/* prints the list of metadata features*/
void print_metadata(se_ostream& crmout);
/* prints the list of documents*/
void print_documents(se_ostream& crmout, bool ps = true);
/* prints the list of documents in the selected collection*/
void print_documents_in_collection(se_ostream& crmout, const char* collection);
/* prints the list of collections*/
void print_collections(se_ostream& crmout, bool ps = true);


#endif /* _CRMUTILS_H */

