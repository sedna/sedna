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
#include "tr/strings/strings.h"
#include "tr/mo/indirection.h"

xptr insert_element(xptr left_sibling, xptr right_sibling, xptr parent, const char* name, xmlscm_type type, xmlns_ptr ns);
xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, const char* value, strsize_t data_size, xmlns_ptr ns);
xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent, xmlns_ptr ns);
xptr insert_comment(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size);
xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent, const char* target, size_t tsize, const char* data, size_t dsize);
xptr insert_doc_node(doc_schema_node_cptr doc_snode, const char * doc_name, const char * collection_name);

xptr insert_text(xptr left_sib, xptr right_sib, xptr parent, const text_source_t source);


extern xptr last_inserted_node_indirection;

inline xptr get_last_mo_inderection() {
    U_ASSERT(last_inserted_node_indirection != XNULL);
    return last_inserted_node_indirection;
}

inline xptr insert_element_i(xptr left_sibling, xptr right_sibling, xptr parent, const char* name, xmlscm_type type, xmlns_ptr ns) {
    insert_element(
            indirectionDereferenceCP(left_sibling),
            indirectionDereferenceCP(right_sibling),
            indirectionDereferenceCP(parent),
            name, type, ns
        );

    return get_last_mo_inderection();
}

inline xptr insert_text_i(xptr left_sib, xptr right_sib, xptr parent, const text_source_t source) {
    insert_text(
            indirectionDereferenceCP(left_sib),
            indirectionDereferenceCP(right_sib),
            indirectionDereferenceCP(parent),
            source
        );

    return get_last_mo_inderection();
}

inline xptr insert_attribute_i(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, const char* value, strsize_t data_size, xmlns_ptr ns) {
    insert_attribute(
            indirectionDereferenceCP(left_sib),
            indirectionDereferenceCP(right_sib),
            indirectionDereferenceCP(parent),
            name, type, value, data_size, ns
        );

    return get_last_mo_inderection();
}

struct delete_context_t {
    const char* doc_name;
    const char* collection_name;

    bool document_delete;
    bool no_index_update;
    bool no_merge_text_nodes;
};

static const delete_context_t delete_node_context = {NULL, NULL, false, false, false};

bool delete_node(xptr node_xptr, const delete_context_t * context);

void delete_doc_node(xptr node, const char * doc_name, const char * collection_name);

#endif /* _MO_H */
