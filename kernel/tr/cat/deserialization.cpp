/*
 * File: deserialization.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/xptr/xptr.h"

#include "tr/cat/catalog.h"
#include "tr/cat/catstore.h"
#include "tr/cat/catmem.h"

#include "tr/structures/schema.h"
#include "tr/structures/metadata.h"
#include "tr/idx/indecies.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#endif


catalog_object * catalog_deserialize_object(xptr p, CatalogMemoryContext* context)
{
    int magic;
    catalog_object * result = NULL;

    cs_pushp();
    magic = cs_get_magic(p);

#define case_create_object(t) case t::magic : \
        {\
            result = new(cat_malloc_context(context, sizeof(t))) t;\
        } break

    switch (magic) {
        case_create_object(xmlns_indb_object);
        case_create_object(metadata_cell_object);
        case_create_object(schema_node_object);
        case_create_object(doc_schema_node_object);
        case_create_object(col_schema_node_object);
        case_create_object(index_cell_object);

#ifdef SE_ENABLE_TRIGGERS
        case_create_object(trigger_cell_object);
#endif // SE_ENABLE_TRIGGERS

#ifdef SE_ENABLE_FTSEARCH
        case_create_object(ft_index_cell_object);
#endif // SE_ENABLE_FTSEARCH

        default : { throw SYSTEM_EXCEPTION("Unkown catalog object serialization was asked for (wrong internal representation version?) \n"); } break;
    }

    result->deserialize(p);

    cs_popp();

    return result;
}

