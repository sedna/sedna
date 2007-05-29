/*
 * File:  PPDropTrigger.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPDropTrigger.h"
#include "tr/executor/base/PPUtils.h"

PPDropTrigger::PPDropTrigger(PPOpIn _trigger_name_, dynamic_context *_cxt_) :	trigger_name(_trigger_name_), cxt(_cxt_)
{
}


PPDropTrigger::~PPDropTrigger()
{
    delete trigger_name.op;
    trigger_name.op = NULL;

    delete cxt;
    cxt = NULL;    
}

void PPDropTrigger::open()
{
    trigger_name.op->open();
}

void PPDropTrigger::close()
{
    trigger_name.op->close();
}

void PPDropTrigger::execute()
{
    tuple_cell tc;
    tuple t(1);
    trigger_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = trigger_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    trigger_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        
    tc = tuple_cell::make_sure_light_atomic(tc);

	trigger_cell::delete_trigger(tc.get_str_mem());
}

