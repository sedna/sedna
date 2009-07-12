/*
 * File:  PPQueryRoot.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/root/PPQueryRoot.h"
#include "tr/crmutils/crmutils.h"
#include "tr/locks/locks.h"


PPQueryRoot::PPQueryRoot(dynamic_context *_cxt_,
                         PPOpIn _child_,
                         t_print _print_mode_) : PPQueryEssence(),
                                                 child(_child_),
                                                 data(_child_.ts),
                                                 cxt(_cxt_),
                                                 print_mode(_print_mode_)
{
}

PPQueryRoot::~PPQueryRoot()
{
    delete child.op;
    child.op = NULL;
    delete cxt;
    cxt = NULL;
}

void PPQueryRoot::open()
{
    local_lock_mrg->lock(lm_s);
	first=true;
    dynamic_context::global_variables_open();
    child.op->open();
	if (print_mode==sxml)
	{
		dynamic_context::add_char_mapping("<","<",-1);
		dynamic_context::add_char_mapping(">",">",-1);
		dynamic_context::add_char_mapping("&","&",-1);
		dynamic_context::add_char_mapping("\"","\\\"",-1);		
		dynamic_context::add_char_mapping("\\","\\\\",-1);
	}
}

void PPQueryRoot::close()
{
    child.op->close();
    dynamic_context::global_variables_close();
}

bool PPQueryRoot::next()
{
    child.op->next(data);

    if (data.is_eos()) return false;

    switch (cxt->st_cxt->output_indent)
    {
        case se_output_indent_yes: print_tuple(data, dynamic_context::ostr(), cxt, print_mode, first, true);  break;
        case se_output_indent_no : print_tuple(data, dynamic_context::ostr(), cxt, print_mode, first, false); break;
        default                  : throw USER_EXCEPTION2(SE1003, "Unexpected se_output_indent");
    }
	
	if (first)
		first = false;

    return true;
}

void PPQueryRoot::execute()
{
    while (next());
}
