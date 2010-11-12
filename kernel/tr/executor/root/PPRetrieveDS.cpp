/*
 * File:  PPRetrieveDS.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPRetrieveDS.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"
#include "tr/tr_globals.h"


PPRetrieveDS::PPRetrieveDS(PPOpIn _name_,
                           dynamic_context *_cxt_,
                           db_entity_type _type_) : PPQueryEssence("PPRetrieveDS"),
                                                    name(_name_),
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

void PPRetrieveDS::do_open()
{
    local_lock_mrg->lock(lm_s);

    cxt->global_variables_open();
    name.op->open();
}

void PPRetrieveDS::do_close()
{
    name.op->close();
    cxt->global_variables_close();
}

void PPRetrieveDS::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    name.op->accept(v);
    v.pop();
}

void PPRetrieveDS::do_execute()
{
    throw USER_EXCEPTION2(SE2901, "Retrieve Document Schema. Please use system documents i.e. doc('$schema_[docname]')");
}

