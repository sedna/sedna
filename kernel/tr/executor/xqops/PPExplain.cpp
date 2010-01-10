/*
 * File:  PPExplain.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPExplain.h"
#include "tr/executor/base/visitor/PPExplainVisitor.h"


PPExplain::PPExplain(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPQueryEssence* _qep_tree_) : PPIterator(_cxt_, _info_),
                                                   qep_tree(_qep_tree_),
                                                   visitor(NULL)
{
}

PPExplain::~PPExplain()
{
}

void PPExplain::do_open ()
{
    first_time = true;
}

void PPExplain::do_reopen()
{
    first_time = true;
}

void PPExplain::do_close()
{
    delete visitor;
    visitor = NULL;
}

void PPExplain::do_next (tuple &t)
{
    if(first_time) 
    {
        first_time = false;

        if(visitor == NULL)
        {
            visitor = new PPExplainVisitor();
            qep_tree->accept(*visitor);
        }

        PPExplainVisitor* evisitor = dynamic_cast<PPExplainVisitor*>( visitor );

        U_ASSERT(evisitor != NULL);
        
        /* result() returns indirection for the doc("$explain") */
        xptr res = evisitor->result();
        t.copy(tuple_cell::node_indir(res));
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPExplain::do_copy(dynamic_context *_cxt_)
{
    throw XQUERY_EXCEPTION2(SE1003, "Copy could not be called for explain function");
}

void PPExplain::do_accept(PPVisitor &v)
{
    throw XQUERY_EXCEPTION2(SE1003, "Accept could not be called for explain function");
}
