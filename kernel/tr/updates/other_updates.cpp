/*
* File:  other_updates.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/updates/updates.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/mo/mo.h"
#include "tr/auth/auc.h"
#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif

// Rename operation
void rename(PPOpIn arg, xsd::NCName name)
{
    // Creating the first sequence (different validity tests+ indirection deref)
    tuple t(arg.ts);
    xptr_sequence argseq;
    arg.op->next(t);
    while (!t.is_eos())
    {
        if (t.cells[0].is_node())
        {
            xptr node=t.cells[0].get_node();
            CHECKP(node);
            if (is_node_persistent(node)&& (is_node_element(node)||is_node_attribute(node)) )
            {
                //xptr indir=nodeGetIndirection(node);
                argseq.add(node);
            }
#ifndef IGNORE_UPDATE_ERRORS
            else
            {
                throw USER_EXCEPTION(SE2020);
            }
#endif
        }
#ifndef IGNORE_UPDATE_ERRORS
        else
        {
            throw USER_EXCEPTION(SE2021);
        }
#endif
        arg.op->next(t);
    }

    if (argseq.size()<=0) return;
    // Checking authorization
    if (is_auth_check_needed(RENAME_STATEMENT))
        auth_for_update(&argseq, RENAME_STATEMENT, false);
    //Sort in document order
    argseq.sort();
    //changing to indirection
    xptr_sequence::iterator it=argseq.begin();
    xptr res = XNULL;
    while (it!=argseq.end())
    {
        xptr node=*it;
        CHECKP(node);
        xptr indir=nodeGetIndirection(node);
        argseq.set(indir,it);
        ++it;
    }
    it=argseq.end();
    do {
        --it;
        xptr indir=*it;
        xptr node=indirectionDereferenceCP(indir);
        CHECKP(node);
        t_item type= getNodeType(node);
        xptr left=node;
        xptr parent= nodeGetParent(node);
        CHECKP(node);
#ifdef SE_ENABLE_TRIGGERS
        // add here triggers on RENAME !!!
        //        if (apply_per_node_triggers(XNULL, XNULL, parent, TRIGGER_BEFORE, TRIGGER_INSERT_EVENT, name, type) == XNULL)
        //    		return;
        //        if (apply_per_node_triggers(XNULL, node, parent, TRIGGER_BEFORE, TRIGGER_DELETE_EVENT) == XNULL)
        //    		return;
#endif

        switch(type) {
        case attribute:
            {
                //1. insert
                text_membuf_t buf(text_source_node(node));
                CHECKP(node);
                res = insert_attribute(left, XNULL, parent, xsd::QName::createNsN(NULL_XMLNS, name.getValue()), AttributeNode(node).getType(), buf.getTextSource());
                //2. delete
                delete_node(indirectionDereferenceCP(indir), &delete_node_context);
                break;
            }
        case element:
            {
                //1.INSERT
                res=insert_element(left, XNULL, parent, name.getValue(), ElementNode(node).getType(), NULL_XMLNS);
                copy_node_content(get_last_mo_inderection(), indirectionDereferenceCP(indir), XNULL, NULL, true);
                //2.DELETE
                delete_node(indirectionDereferenceCP(indir), &delete_node_context);
                break;
            }
        default:
            throw SYSTEM_EXCEPTION("Unexpected node type in rename operation");

        }
#ifdef SE_ENABLE_TRIGGERS
        apply_per_node_triggers(res, XNULL, parent, XNULL, TRIGGER_AFTER, TRIGGER_INSERT_EVENT);
#endif
        if (it==argseq.begin()) break;
    } while (true);
#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif
}
