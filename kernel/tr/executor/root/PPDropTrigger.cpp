/*
 * File:  PPDropTrigger.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropTrigger.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/triggers/triggers.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"

PPDropTrigger::PPDropTrigger(PPOpIn _trigger_name_,
                             dynamic_context *_cxt_) : PPUpdate("PPDropTrigger"),
                                                       trigger_name(_trigger_name_),
                                                       cxt(_cxt_)
{
}


PPDropTrigger::~PPDropTrigger()
{
    delete trigger_name.op;
    trigger_name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropTrigger::do_open()
{
    cxt->global_variables_open();
    trigger_name.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPDropTrigger::do_close()
{
    trigger_name.op->close();
    cxt->global_variables_close();
}

void PPDropTrigger::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    trigger_name.op->accept(v);
    v.pop();
}

void PPDropTrigger::do_execute()
{
    tuple_cell tc;
    xqp_tuple t(1);
    trigger_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = trigger_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    trigger_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = tuple_cell::make_sure_light_atomic(tc);

    local_lock_mrg->put_lock_on_trigger(tc.get_str_mem());

    if(find_trigger(tc.get_str_mem()) == XNULL)
        throw USER_EXCEPTION2(SE3211, (std::string("Trigger '") + tc.get_str_mem() + "'").c_str());

    auth_for_drop_object(tc.get_str_mem(), "trigger", false);

    delete_trigger(tc.get_str_mem());
}

