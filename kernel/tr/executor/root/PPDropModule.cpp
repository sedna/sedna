/*
 * File:  PPDropIndex.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropModule.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/mo/mo.h"
#include "tr/structures/metadata.h"
#include "tr/auth/auc.h"

PPDropModule::PPDropModule(PPOpIn _module_name_) : module_name(_module_name_)
{
}

PPDropModule::~PPDropModule()
{
    delete module_name.op;
    module_name.op = NULL;
}

void PPDropModule::open()
{
    local_lock_mrg->lock(lm_x); // because Leon changes the descriptive schema of the document/collection
    module_name.op->open();
}

void PPDropModule::close()
{
    module_name.op->close();
}

void PPDropModule::accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    module_name.op->accept(v);
    v.pop();
}

void PPDropModule::execute()
{
    tuple_cell tc;
    tuple t(1);
    module_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = module_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    module_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_collection(MODULES_COLLECTION_NAME);
    auth_for_drop_object(tc.get_str_mem(), "module", false);
    try
    {
        delete_document_from_collection(MODULES_COLLECTION_NAME, tc.get_str_mem());
    }
    catch(SednaUserException& e)
    {
        if(e.get_code() == SE2006)
        {
            throw USER_EXCEPTION2(SE1074, tc.get_str_mem());
        }
        else
        {
            throw;
        }
    }
}

