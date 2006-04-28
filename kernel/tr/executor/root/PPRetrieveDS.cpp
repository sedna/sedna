/*
 * File:  PPRetrieveDS.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPRetrieveDS.h"
#include "crmutils.h"
#include "locks.h"
#include "auc.h"


PPRetrieveDS::PPRetrieveDS(PPOpIn _name_,
                           db_entity_type _type_,
                           se_ostream& _s_) : name(_name_),
                                              type(_type_),
                                              s(_s_)
{
}

PPRetrieveDS::~PPRetrieveDS()
{
    delete name.op;
    name.op = NULL;
}

void PPRetrieveDS::open()
{
    local_lock_mrg->lock(lm_s);

    name.op->open();
}

void PPRetrieveDS::close()
{
    name.op->close();
}

void PPRetrieveDS::execute()
{
    tuple_cell tc;
    tuple t(1);
    name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        
    tc = tuple_cell::make_sure_light_atomic(tc);


    counted_ptr<db_entity> db_ent(new db_entity);

    if (type == dbe_document)
    { /// dbe_document
    	db_ent->name = new char[tc.get_strlen_mem() + 1];
    	strcpy(db_ent->name, tc.get_str_mem());
    	db_ent->type = dbe_document;
        auth_for_query(db_ent);
        local_lock_mrg->put_lock_on_document(tc.get_str_mem());
        print_descriptive_schema(tc.get_str_mem(), s);
    }
    else
    { /// dbe_collection
    	db_ent->name = new char[tc.get_strlen_mem() + 1];
    	strcpy(db_ent->name, tc.get_str_mem());
    	db_ent->type = dbe_collection;
        auth_for_query(db_ent);
        local_lock_mrg->put_lock_on_collection(tc.get_str_mem());
        print_descriptive_schema_col(tc.get_str_mem(), s);
    }
}

