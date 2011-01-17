/*
 * File:  index_types.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDEX_TYPES_H
#define _INDEX_TYPES_H

#include "common/sedna.h"

#include "tr/cat/catptr.h"

struct PathExpr;

typedef const char * lockid_t; /* FIXME: implement this! AND MOVE TO OTHER HEADER */

void index_on_session_begin();
void index_on_session_end();

enum index_backend_t { index_btree, index_bstrie };

index_backend_t str2index_type(const char * str);

struct index_descriptor_t {
    const char * index_title;
    metadata_cell_xptr owner;
    xmlscm_type keytype;
    index_backend_t backend_type;

    PathExpr *object; /* persistent special */  // absolute xPath expression for object nodes xPath
    PathExpr *key; /* persistent special */     // relative xPath expression for key value nodes
};

#endif /* _INDEX_TYPES_H */
