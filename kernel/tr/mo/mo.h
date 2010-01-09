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

class text_cptr;

xptr insert_element(xptr left_sibling, xptr right_sibling, xptr parent, const char* name, xmlscm_type type, xmlns_ptr ns);
xptr insert_text(xptr left_sib, xptr right_sib, xptr parent, const void* value, strsize_t size, text_type ttype = text_mem);
xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent, const char* name, xmlscm_type type, const char* value, int data_size, xmlns_ptr ns);
xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent, xmlns_ptr ns);
xptr insert_comment(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size);
xptr insert_cdata(xptr left_sib, xptr right_sib, xptr parent, const char* value, strsize_t size);
xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent, const char* target, int tsize, const char* data, strsize_t dsize);
xptr insert_doc_node(doc_schema_node_cptr doc_snode, const char * doc_name, const char * collection_name);

struct doc_info_t;

bool delete_node(xptr node_xptr, const doc_info_t * doc_info = NULL, bool no_index_update = false);
void delete_doc_node(xptr node, const char * doc_name, const char * collection_name);

extern xptr last_inserted_node_indirection;

inline xptr get_last_mo_inderection() { return last_inserted_node_indirection; }

class text_cptr {
    struct counted_str {
        int counter;
        size_t size;
        char text[0];
    } * target;

    inline void release() {
        if (target != NULL) {
            if (--target->counter == 0) {
                se_free(target);
            }
            target = NULL;
        }
    }

    inline void acquire(counted_str * c) {
        target = c;
        if (target != NULL) { ++target->counter; }
    }

public :
    inline explicit text_cptr(xptr node) : target(NULL) {
        CHECKP(node);
        t_dsc * dsc = T_DSC(node);

        if (isTextEmpty(dsc)) {
            target = NULL;
        } else if (isPstrLong(dsc)) {
            // TODO!
        } else {
            size_t size = (size_t) getTextSize(dsc);
            target = (counted_str *) se_alloc(sizeof(counted_str) + size);
            target->counter = 1;
            target->size = size;
            if (size > 0) {
                textCopyToBuffer(target->text, dsc);
            }
        }
    }

    text_cptr(const text_cptr &p) : target(p.target) { acquire(p.target); };

    text_cptr & operator=(const text_cptr & r) {
        if (this != &r) {
            release();
            acquire(r.target);
        }
        return *this;
    }

    ~text_cptr() { release(); }

    char * get() const { return target == NULL ? NULL : target->text; };
    size_t getSize() const { return target == NULL ? 0 : target->size; };
};

#endif /* _MO_H */
