/*
* File:  deletions.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/updates/updates.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/mo/mo.h"
#include "tr/auth/auc.h"
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif

void delete_undeep(PPOpIn arg)
{
    // Creating the first sequence (different validity tests+ indirection deref)
    xqp_tuple t(arg.ts);
    xptr_sequence argseq;
    arg.op->next(t);
    while (!t.is_eos())
    {
        if (t.cells[0].is_node())
        {
            xptr node=t.cells[0].get_node();
            CHECKP(node);
            if (is_node_persistent(node)&& !is_node_document(node))
            {
                //xptr indir=nodeGetIndirection(node);
                argseq.add(node);
            }
#ifndef IGNORE_UPDATE_ERRORS
            else
            {
                throw USER_EXCEPTION(SE2010);
            }
#endif
        }
#ifndef IGNORE_UPDATE_ERRORS
        else
        {
            throw USER_EXCEPTION(SE2011);
        }
#endif
        arg.op->next(t);
    }

    //Sort in document order
    if (argseq.size()<=0) return;
    argseq.sort();
    // INDIR
    xptr_sequence::iterator it=argseq.begin();
    xptr node;
    while (it!=argseq.end())
    {
        node=*it;
        CHECKP(node);
        xptr indir=nodeGetIndirection(node);
        argseq.set(indir,it);
        it++;
    }
    // Checking authorization
    if (is_auth_check_needed(DELETE_STATEMENT))
        auth_for_update(&argseq, DELETE_STATEMENT, false);

    //  cycle on  sequence
#ifdef SE_ENABLE_TRIGGERS
    apply_per_statement_triggers(&argseq, false, NULL, false, TRIGGER_BEFORE, TRIGGER_DELETE_EVENT);
#endif
    it=argseq.end();
    do
    {
        --it;
        xptr node=indirectionDereferenceCP(*it);
        CHECKP(node);
        {
            t_item type=getNodeType(node);
            switch(type)
            {
            case attribute: case text: case comment: case pr_ins:
                {
                    delete_node(node, &delete_node_context);
                    break;
                }
            case element:
                {
                    //1.INSERT
                    copy_node_content(nodeGetParentIndirection(node), node, getIndirectionSafeCP(node), NULL, true);
                    //2.DELETE
                    delete_node(indirectionDereferenceCP(*it), &delete_node_context);
                    break;
                }
            default:
                throw SYSTEM_EXCEPTION("Unexpected node type in delete undeep");
            }
        }

        if (it==argseq.begin()) break;
    }
    while (true);
#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif
#ifdef SE_ENABLE_TRIGGERS
    apply_per_statement_triggers(NULL, false, NULL, false, TRIGGER_AFTER, TRIGGER_DELETE_EVENT);
#endif
}
void delete_deep(PPOpIn arg)
{
    // Creating the first sequence (different validity tests+ indirection deref)
    /*xptr blk(0,(void*)0x33fd0000);
    CHECKP(blk)
    {shft hh_size=HHSIZE(blk);
    for (int i=0; i<hh_size; i++)
    {
    hh_slot* tmp = (hh_slot*)HH_ADDR(blk, i);
    if (tmp->hole_shft+ tmp->hole_size==SSB(blk) )
    {
    throw SYSTEM_EXCEPTION("[pstr_deallocate()] string can not be adjacent with with SS tail and with some hole on the right simultaneously");
    }
    }
    }*/
    xqp_tuple t(arg.ts);
    xptr_sequence argseq;
    arg.op->next(t);
    while (!t.is_eos())
    {
        if (t.cells[0].is_node())
        {
            xptr node=t.cells[0].get_node();
            //			node.print();
            CHECKP(node);
            if (is_node_persistent(node) ) argseq.add(node);
#ifndef IGNORE_UPDATE_ERRORS
            else
            {
                throw USER_EXCEPTION(SE2012);
            }
#endif
        }
#ifndef IGNORE_UPDATE_ERRORS
        else
        {
            throw USER_EXCEPTION(SE2011);
        }
#endif
        arg.op->next(t);
    }

    // Checking authorization
    if (is_auth_check_needed(DELETE_STATEMENT))
        auth_for_update(&argseq, DELETE_STATEMENT, true);

    //Sort in document order
    if (argseq.size()<=0) return;

    //!!! debug
    /*
    xptr_sequence::iterator my_it;
    for (my_it = argseq.begin(); my_it != argseq.end(); my_it++)
    {
    xptr p = argseq.get(my_it);
    p.print();
    }
    */
#ifdef SE_ENABLE_TRIGGERS
    apply_per_statement_triggers(&argseq, true, NULL, false, TRIGGER_BEFORE, TRIGGER_DELETE_EVENT);
#endif

    argseq.sort();
    //  cycle on  sequence
    xptr_sequence::iterator it=argseq.begin();
    bool mark=false;
    do {
        xptr node=*it;
        do {
            if (it+1==argseq.end()) { mark=true; break;}
            ++it;
        } while ((node == *it) || nid_ancestor(node,*it));
        delete_node(node, &delete_node_context);
    } while (!mark);
#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif
#ifdef SE_ENABLE_TRIGGERS
    apply_per_statement_triggers(NULL, false, NULL, false, TRIGGER_AFTER, TRIGGER_DELETE_EVENT);
#endif
}
