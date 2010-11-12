/*
 * File:  tr_base.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 * Contains common trn module type definitions. Usually if type is defined here
 * it means that it is used exactly within trn and shared among several trn
 * subparts.
 */


#ifndef _TR_BASE_H
#define _TR_BASE_H

#include <string>
#include "common/sedna.h"
#include "tr/structures/nodetypes.h"

/*
 * Type of query that executed by query processor
 */
enum QueryType
{
    TL_XQuery       = 9,    // XQuery query (xq_string -> AST -> full analysis -> QEP)
    TL_XQueryMod    = 8,    // XQuery query for modules (without further lreturn; only sema; xq_string -> AST -> sema)
    TL_ASTInitial   = 7,    // internal initial (before semantic pass) AST representation (ast_string -> AST -> full analysis -> QEP)
    TL_ASTQEPReady  = 6,    // internal QEP-ready AST representation (we wont run any analysis; ast_string -> AST -> QEP)
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

#endif /* _TR_BASE_H */

