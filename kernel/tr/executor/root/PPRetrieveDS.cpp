/*
 * File:  PPRetrieveDS.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPRetrieveDS.h"
#include "tr/crmutils/crmutils.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"


PPRetrieveDS::PPRetrieveDS(PPOpIn _name_,
                           dynamic_context *_cxt_,
                           db_entity_type _type_) : name(_name_),
                                                    cxt(_cxt_),
                                                    type(_type_)
{
}

PPRetrieveDS::~PPRetrieveDS()
{
    delete name.op;
    name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPRetrieveDS::open()
{
    local_lock_mrg->lock(lm_s);

    dynamic_context::global_variables_open();
    name.op->open();
}

void PPRetrieveDS::close()
{
    name.op->close();
    dynamic_context::global_variables_close();
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


    counted_ptr<db_entity> db_ent(se_new db_entity);

    if (type == dbe_document)
    { /// dbe_document
    	db_ent->name = se_new char[tc.get_strlen_mem() + 1];
    	strcpy(db_ent->name, tc.get_str_mem());
    	db_ent->type = dbe_document;
        auth_for_query(db_ent);
        local_lock_mrg->put_lock_on_document(tc.get_str_mem());
        print_descriptive_schema(tc.get_str_mem(), dynamic_context::ostr());
    }
    else
    { /// dbe_collection
    	db_ent->name = se_new char[tc.get_strlen_mem() + 1];
    	strcpy(db_ent->name, tc.get_str_mem());
    	db_ent->type = dbe_collection;
        auth_for_query(db_ent);
        local_lock_mrg->put_lock_on_collection(tc.get_str_mem());
        print_descriptive_schema_col(tc.get_str_mem(), dynamic_context::ostr());
    }
}

