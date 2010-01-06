/*
 * File:  PPCreateIndex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPCreateIndex.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPVisitor.h"
#include "tr/idx/indexes.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"

PPCreateIndex::PPCreateIndex(PathExpr *_object_path_,
                             PathExpr *_key_path_,
                             xmlscm_type _key_type_,
                             counted_ptr<db_entity> _db_ent_,
                             PPOpIn _index_name_,
                             dynamic_context *_cxt_) :
                                                    root(XNULL),
                                                    object_path(_object_path_),
                                                    key_path(_key_path_),
                                                    key_type(_key_type_),
                                                    db_ent(_db_ent_),
                                                    index_name(_index_name_),
                                                    cxt(_cxt_)
{
}

PPCreateIndex::~PPCreateIndex()
{
    delete index_name.op;
    index_name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPCreateIndex::open()
{
    local_lock_mrg->lock(lm_x); // because Leon changes the descriptive schema of the document/collection
    root = get_schema_node(db_ent, "Unknown entity passed to PPCreateIndex");
    dynamic_context::global_variables_open();
    index_name.op->open();
}

void PPCreateIndex::close()
{
    index_name.op->close();
    dynamic_context::global_variables_close();
    root = XNULL;
}

void PPCreateIndex::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    index_name.op->accept(v);    
    v.pop();
}

void PPCreateIndex::execute()
{
    tuple_cell tc;
    tuple t(1);
    index_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = index_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    index_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_index(tc.get_str_mem());

    auth_for_create_index(tc.get_str_mem(), db_ent->name, db_ent->type == dbe_collection);

    create_index(object_path,
                 key_path,
                 key_type,
                 root,
                 tc.get_str_mem(),
                 db_ent->name,
                 (db_ent->type == dbe_document));

}

