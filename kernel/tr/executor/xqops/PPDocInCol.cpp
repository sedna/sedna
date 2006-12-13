/*
 * File:  PPDocInCol.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPDocInCol.h"
#include "casting_operations.h"
#include "metadata.h"
#include "PPUtils.h"

PPDocInCol::PPDocInCol(dynamic_context *_cxt_, 
                       PPOpIn _col_name_op_,
                       PPOpIn _doc_name_op_) : PPIterator(_cxt_),
                                               col_name_op(_col_name_op_),
                                               doc_name_op(_doc_name_op_)
{
}

PPDocInCol::~PPDocInCol()
{
    delete col_name_op.op;
    col_name_op.op = NULL;
    delete doc_name_op.op;
    doc_name_op.op = NULL;
}

void PPDocInCol::open ()
{
    col_name_op.op->open();
    doc_name_op.op->open();

    first_time = true;
}


void PPDocInCol::reopen()
{
    col_name_op.op->reopen();
    doc_name_op.op->reopen();

    first_time = true;
}


void PPDocInCol::close ()
{
    col_name_op.op->close();
    doc_name_op.op->close();
}

void PPDocInCol::next(tuple &t)
{
    if (first_time)
    {
        col_name_op.op->next(t);
        if (t.is_eos()) return;

        tuple_cell tc_col = cast(col_name_op.get(t), xs_string);
        tc_col = tuple_cell::make_sure_light_atomic(tc_col);

        col_name_op.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION(FODC0005);

        doc_name_op.op->next(t);
        if (t.is_eos()) return;

        tuple_cell tc_doc = cast(doc_name_op.get(t), xs_string);
        tc_doc = tuple_cell::make_sure_light_atomic(tc_doc);

        doc_name_op.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION(FODC0005);

        first_time = false;

        // Put lock on collection and check security
		counted_ptr<db_entity> db_ent(new db_entity);
        db_ent->name = new char[tc_col.get_strlen_mem() + 1];
        strcpy(db_ent->name, tc_col.get_str_mem());
		db_ent->type = dbe_collection;
        schema_node *root = get_schema_node(db_ent, "Unknown entity passed to PPDocInCol");

        xptr res = find_document((const char*)tc_col.get_str_mem(), (const char*)tc_doc.get_str_mem());
        if (res == NULL)
        {
            throw USER_EXCEPTION2(SE1006, (std::string("Document '") + 
                                           tc_doc.get_str_mem() + 
                                           "' in collection '" + 
                                           tc_col.get_str_mem() + "'").c_str());
        }

        t.copy(tuple_cell::node(res));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDocInCol::copy(dynamic_context *_cxt_)
{
    PPDocInCol *res = new PPDocInCol(_cxt_, col_name_op, doc_name_op);
    res->col_name_op.op = col_name_op.op->copy(_cxt_);
    res->doc_name_op.op = doc_name_op.op->copy(_cxt_);
    return res;
}

bool PPDocInCol::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    return true;
}



