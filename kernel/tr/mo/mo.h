/*
 * File:  mo.h
 *
 * This file provides interfaces for a high level internal data representation manipulations (delete and insert)
 *
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _MO_H
#define _MO_H

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/tr_base.h"
#include "tr/structures/schema.h"
#include "tr/strings/strings_base.h"

xptr insert_element(xptr left_sibling, xptr right_sibling, xptr parent, const char* name, xmlscm_type type, xmlns_ptr ns);
xptr insert_text(xptr left_sib, xptr right_sib, xptr parent, const void* value, strsize_t size, text_type ttype = text_mem);
xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, const char* value, int data_size, xmlns_ptr ns);
xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent, xmlns_ptr ns);
xptr insert_comment(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size);
xptr insert_cdata(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size);
xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent, const char* target, int tsize, const char* data, strsize_t dsize);
xptr insert_doc_node(doc_schema_node_cptr doc_snode, const char * doc_name);

bool delete_node(xptr node, bool is_drop_document = false);
void delete_doc_node(xptr node);


#endif /* _MO_H */
 