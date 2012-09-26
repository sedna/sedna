/*
 * File:  PPDocInCol.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPDocInCol.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/structures/metadata.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPDocInCol::PPDocInCol(dynamic_context *_cxt_,
                       operation_info _info_, 
                       PPOpIn _col_name_op_,
                       PPOpIn _doc_name_op_) : PPIterator(_cxt_, _info_, "PPDocInCol"),
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

void PPDocInCol::do_open ()
{
    col_name_op.op->open();
    doc_name_op.op->open();

    first_time = true;
}


void PPDocInCol::do_reopen()
{
    col_name_op.op->reopen();
    doc_name_op.op->reopen();

    first_time = true;
}


void PPDocInCol::do_close()
{
    col_name_op.op->close();
    doc_name_op.op->close();
}

void PPDocInCol::do_next(tuple &t)
{
    if (first_time)
    {
        tuple_cell tc_col = get_name_from_PPOpIn(col_name_op, "collection", "fn:doc(, )", true);
        if (tc_col.is_eos()) 
        {
            t.set_eos();
            return;
        }

        tuple_cell tc_doc = get_name_from_PPOpIn(doc_name_op, "document", "fn:doc(, )", true);
        if (tc_doc.is_eos())
        {
            t.set_eos();
            return;
        }

        first_time = false;

		counted_ptr<db_entity> db_ent(new db_entity);
        db_ent->name = new char[tc_col.get_strlen_mem() + 1];
        strcpy(db_ent->name, tc_col.get_str_mem());
		db_ent->type = dbe_collection;
        schema_node_cptr root = get_schema_node(db_ent, (std::string("Unknown entity passed to fn:doc( , ): ") + db_ent->name).c_str());

        bool valid;
        Uri::check_constraints(&tc_doc, &valid, NULL);
        if(!valid) throw XQUERY_EXCEPTION2(FODC0005, "Invalid uri in the first argument of fn:doc( , ).");

        xptr res = find_document_in_collection((const char*)tc_col.get_str_mem(), (const char*)tc_doc.get_str_mem());
        if (res == XNULL)
        {
            throw XQUERY_EXCEPTION2(SE1006, (std::string("Document '") + 
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

PPIterator* PPDocInCol::do_copy(dynamic_context *_cxt_)
{
    PPDocInCol *res = new PPDocInCol(_cxt_, info, col_name_op, doc_name_op);
    res->col_name_op.op = col_name_op.op->copy(_cxt_);
    res->doc_name_op.op = doc_name_op.op->copy(_cxt_);
    return res;
}

void PPDocInCol::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    col_name_op.op->accept(v);
    doc_name_op.op->accept(v);
    v.pop();
}
