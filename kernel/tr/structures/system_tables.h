/*
 * File:  system_tables.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SYSTEM_TABLES_H
#define _SYSTEM_TABLES_H

#include <string>

#include "common/sedna.h"
#include "common/counted_ptr.h"

#include "tr/cat/catptr.h"

/* Type of the entity that is stored in database */
enum db_entity_type { dbe_document,		// document
                      dbe_collection,	// collection
                      dbe_module        // module
                    };

/* Database entity */
struct db_entity
{
    db_entity_type type;		// type of the db entity
    char *name;					// name of the db entity
    
    ~db_entity() { delete [] name; name = NULL; }
    
    inline std::string to_string() const
    {
        std::string res;
        switch(type)
        {
        case dbe_document: res += "document("; break;
        case dbe_collection: res += "collection("; break;
        case dbe_module: res += "module("; break;
        throw USER_EXCEPTION2(SE1003, "Impossible type in database entry to string conversion");
        }
        if(name != NULL) res += name;
        res += ")";
        return res;
    }
};


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



/* The following methods return DT_NON_SYSTEM if 
 * given name is not one of the reserved.
 */
document_type get_document_type(counted_ptr<db_entity> db_ent);
document_type get_document_type(const char* title, db_entity_type type);


schema_node_xptr get_system_doc(document_type type, const char* title);
void system_tables_on_kernel_statement_end();



#endif /* _SYSTEM_TABLES_H */
