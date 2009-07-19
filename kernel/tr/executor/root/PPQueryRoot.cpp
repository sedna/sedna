/*
 * File:  PPQueryRoot.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/root/PPQueryRoot.h"
#include "tr/crmutils/crmutils.h"
#include "tr/locks/locks.h"
#include "tr/tr_globals.h"


PPQueryRoot::PPQueryRoot(dynamic_context *_cxt_,
                         PPOpIn _child_) :       PPQueryEssence(),
                                                 child(_child_),
                                                 data(_child_.ts),
                                                 cxt(_cxt_),
                                                 print_mode(xml),
                                                 output_stream(NULL)
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
    first = true;
    dynamic_context::global_variables_open();
    child.op->open();
	
    print_mode    = tr_globals::client->get_result_type();
    output_stream = tr_globals::client->get_se_ostream();

    if (print_mode == sxml)
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

    if(data.cells_number != 1) 
        throw XQUERY_EXCEPTION2(SE1003, "Result of the query contains nested sequences");
    
    tuple_cell tc = data.cells[0];

    t_item nt = element;   /* nt will be ignored if tuple cell contains atomic value */
    xmlscm_type st = 0;    /* by default there is no type (anyType) */
    bool is_node;
    
    if(is_node = tc.is_node())
    {
        xptr node = tc.get_node();
        CHECKP(node);
        nt = GETTYPE(GETSCHEMENODEX(node));
        if(nt == element) {
            st = ((e_dsc*)XADDR(node))->type;
        }
        else if (nt == attribute)  {
            st = ((a_dsc*)XADDR(node))->type;
        }
    }
    else {
        st = tc.get_atomic_type();
    }
    
    tr_globals::client->begin_item(!is_node, st, nt);

    if(tr_globals::client->supports_serialization()) 
    {
        /* If client supports serialization we don't have to
         * make indentation, space delimiting and so on */
        print_tuple(data, *output_stream, cxt, print_mode, true, false);
    }
    else
    {
        switch (cxt->st_cxt->output_indent)
        {
            case se_output_indent_yes: print_tuple(data, *output_stream, cxt, print_mode, first, true);  break;
            case se_output_indent_no : print_tuple(data, *output_stream, cxt, print_mode, first, false); break;
            default                  : throw USER_EXCEPTION2(SE1003, "Unexpected se_output_indent");
        }
	}
    
    if (first)
        first = false;

    return true;
}

void PPQueryRoot::execute()
{
    while (next());
}
