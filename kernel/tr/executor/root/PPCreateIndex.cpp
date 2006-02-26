/*
 * File:  PPCreateIndex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPCreateIndex.h"
#include "PPUtils.h"
#include "indexes.h"
#include "locks.h"


PPCreateIndex::PPCreateIndex(PathExpr *_object_path_,
                             PathExpr *_key_path_,
                             xmlscm_type _key_type_,
                             counted_ptr<db_entity> _db_ent_,
                             PPOpIn _index_name_) : object_path(_object_path_),
                                                    key_path(_key_path_),
                                                    key_type(_key_type_),
                                                    db_ent(_db_ent_),
                                                    index_name(_index_name_)
{
    root = NULL;
}

PPCreateIndex::~PPCreateIndex()
{
    delete index_name.op;
    index_name.op = NULL;
}

void PPCreateIndex::open()
{
    local_lock_mrg->lock(lm_x); // because Leon changes the descriptive schema of the document/collection
    root = get_schema_node(db_ent, "Unknown entity passed to PPCreateIndex");
    index_name.op->open();
}

void PPCreateIndex::close()
{
    index_name.op->close();
    root = NULL;
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

    index_cell* idc = create_index(object_path, 
                                   key_path, 
                                   key_type, 
                                   (doc_schema_node*)root, 
                                   tc.get_str_mem(),
                                   db_ent->name,
                                   (db_ent->type == dbe_document));

}

