/*
 * File:  other_updates.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/updates/updates.h"
#include "tr/executor/base/xptr_sequence.h"
#include "tr/mo/mo.h"
#include "tr/auth/auc.h"
#include "tr/locks/locks.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers.h"
#endif

#include "tr/structures/nodeutils.h"

void replace(PPOpIn arg)
{
    xptr node, tmp_node, attr_node;
    schema_node_xptr scm_node;
    tuple t(arg.ts);

    xptr_sequence arg1seq;        // Indirection of nodes which are going to be replaced
    xptr_sequence arg1seq_tmp;    // Nodes which are going to be replaced
    xptr_sequence arg2seq;        // Nodes to replace with (both persistent and temp)

    /* Persistent nodes to replace with (+ theirs position in arg2seq) */
    descript_sequence arg3seq(2);

    upd_ns_map* ins_swiz = NULL;
    bool is_node_updated = true;

    /*
     * Fill up sequences with nodes to update and update with,
     * child (arg) returns the following sequence of items:
     * 1. node to be replaced (1)
     * 2. nodes to replace with (2)
     * 3. special tuple which contains separator value (3)
     */
    arg.op->next(t);

    while (!t.is_eos())
    {
        if (t.cells[0].is_node())
        {
            node = t.cells[0].get_node();
            CHECKP(node);
            /*
             * In (1) case node must be persistent (is_node_updated is true)
             * In (2) case it can be temporary
             * In both cases document nodes are not allowed
             */
            if ((!is_node_updated || is_node_persistent(node)) && !is_node_document(node))
            {
                xptr indir = nodeGetIndirection(node);

                if (is_node_updated)
                {
                    /* Case (1) - fill up sequence with nodes to be replaced */
                    is_node_updated=false;
                    /* Next nodes from arg are case (2) nodes, so we can use shared lock */
                    local_lock_mrg->lock(lm_s);
                    arg1seq.add(indir);
                    arg1seq_tmp.add(node);
                }
                else
                {
                    /* Case (2) - fill up sequence with nodes to replace with */
                    if (is_node_persistent(node))
                    {
                        tuple tup(2);
                        tup.copy(tuple_cell::node(node),tuple_cell((int64_t)(arg2seq.size())));
                        arg3seq.add(tup);
                    }
                    arg2seq.add(indir);
                }
            }
#ifndef IGNORE_UPDATE_ERRORS
            else
            {
                throw USER_EXCEPTION(SE2020);
            }
#endif
        }
        else
        {
            /* Must be separator in this case (3) */
            if (t.cells[0].get_atomic_type() == se_separator)
            {
                arg2seq.add(XNULL);
                is_node_updated=true;
                /* Next nodes from arg are case (1) node, so we can use shared lock */
                local_lock_mrg->lock(lm_x);
            }
#ifndef IGNORE_UPDATE_ERRORS
            else throw USER_EXCEPTION(SE2021);
#endif
        }

        arg.op->next(t);
    }

    /* Nothing to do in this case */
    if (arg1seq.size()<=0) return;

    /* Checking authorization */
    if (is_auth_check_needed(REPLACE_STATEMENT))
        auth_for_update(&arg1seq, REPLACE_STATEMENT, false);

    /* Find all common nodes in agr3seq (nodes to replace with) and
     * arg1seq_tmp (nodes to be replaced). Make a copy of all such nodes. */
    arg1seq_tmp.sort();
    arg3seq.sort();
    descript_sequence::iterator it3 = arg3seq.begin();
    xptr_sequence::iterator     it1 = arg1seq_tmp.begin();

    while(it3 != arg3seq.end() && it1 != arg1seq_tmp.end())
    {
        switch(nid_cmp_effective((*it3).cells[0].get_node(), *it1))
        {
        case 0: case -2:
            {
                node = copy_to_temp((*it3).cells[0].get_node());
                xptr indir=nodeGetIndirection(node);
                arg2seq.set(indir,(*it3).cells[1].get_xs_integer());
                ++it3;
            }
            break;
        case 1:
            ++it1;
            break;
        case 2:
            ++it1;
            break;
        case -1:
            ++it3;
            break;
        }
    }
#ifdef SE_ENABLE_TRIGGERS
    apply_per_statement_triggers(&arg1seq, false, NULL, false, TRIGGER_BEFORE, TRIGGER_REPLACE_EVENT);
#endif

    arg3seq.clear();
    xptr_sequence::iterator it  = arg1seq.begin();
    xptr_sequence::iterator sit = arg2seq.begin();
    int ctr=0;
    do
    {
        tuple tup(2);
        /* arg3seq will contain pairs: node -> int, namely
        * node to be replaced -> place in sequence of nodes to replace with */
        tup.copy(tuple_cell::node(indirectionDereferenceCP(*it)),tuple_cell((int64_t)ctr));
        arg3seq.add(tup);
        /* XNULL separates nodes in arg2seq (nodes replace with) per each
        * node in arg1seq (nodes to be replaced) */
        while(*sit!=XNULL)
        {
            sit++;
            ctr++;
        }
        sit++;
        ctr++;
        it++;
    } while (it != arg1seq.end());

    arg3seq.sort();
    it3=arg3seq.begin();
    descript_sequence arg4seq(2);
    do
    {
        node = (*it3).cells[0].get_node();
        tuple t = (*it3);
        t.cells[0].set_safenode(node);
        ++it3;
        arg4seq.add(t);

    } while (it3!=arg3seq.end());

    /* Deleting, inserting new nodes */
    it3 = arg4seq.end();
    do
    {
        --it3;
        node = (*it3).cells[0].get_safenode();
        int pos = (*it3).cells[1].get_xs_integer();
        sit = arg2seq.begin() + pos;
        CHECKP(node);
        xptr leftn   = nodeGetLeftSibling(node);
        xptr rightn  = nodeGetRightSibling(node);
        xptr par_ind = nodeGetParentIndirection(node);
        bool a_m = is_node_attribute(node);
        bool d_m = a_m || is_node_text(node);

#ifdef SE_ENABLE_TRIGGERS
        scm_node = getSchemaPointer(node);
        tmp_node = prepare_old_node(node, scm_node, TRIGGER_REPLACE_EVENT);

        /* Before-for-each-node triggers (cycle for all inserted nodes) */
        xptr_sequence::iterator tr_it = sit;
        while(*tr_it != XNULL)
        {
            if(apply_per_node_triggers(indirectionDereferenceCP(*tr_it),
                                       node,
                                       indirectionDereferenceCP(par_ind),
                                       scm_node,
                                       TRIGGER_BEFORE,
                                       TRIGGER_REPLACE_EVENT) == XNULL)
            {
                goto next_replacement;
            }
            tr_it++;
        }
#endif /* SE_ENABLE_TRIGGERS */

        //pre_deletion
        if (d_m)
        {
            delete_node(node, &delete_node_context);
        }
        //1.inserting attributes from sequence
        while(*sit != XNULL)
        {
            xptr node_child = indirectionDereferenceCP(*sit);
            CHECKP(node_child);
            if (is_node_attribute(node_child))
            {
                attr_node = deep_copy_node(XNULL, XNULL, indirectionDereferenceCP(par_ind), node_child, is_node_persistent(node_child) ? NULL : &ins_swiz, true);
#ifdef SE_ENABLE_TRIGGERS
                apply_per_node_triggers(attr_node, tmp_node, indirectionDereferenceCP(par_ind), scm_node, TRIGGER_AFTER, TRIGGER_REPLACE_EVENT);
#endif
            }
            sit++;
        }
        //2. finding place of insertion
        if (a_m)
        {
            node = getFirstChildNode(indirectionDereferenceCP(par_ind));
            if (node != XNULL)
            {
                CHECKP(node);
                if (is_node_element(node))
                {
                    rightn=node;
                    node=XNULL;
                }
                else
                {
                    rightn=XNULL;
                }
            }
        }
        else
        {
            if (d_m)
            {
                if (rightn==XNULL)
                    node=leftn;
                else
                    node=XNULL;
            }
        }
        //3.main insert cycle
        sit = arg2seq.begin() + pos;
        while(*sit != XNULL)
        {
            xptr node_child = indirectionDereferenceCP(*sit);
            CHECKP(node_child);
            if (!is_node_attribute(node_child))
            {
                node = deep_copy_node(node, rightn, indirectionDereferenceCP(par_ind), node_child, is_node_persistent(node_child) ? NULL : &ins_swiz, true);
#ifdef SE_ENABLE_TRIGGERS
                apply_per_node_triggers(node, tmp_node, indirectionDereferenceCP(par_ind), scm_node, TRIGGER_AFTER, TRIGGER_REPLACE_EVENT);
#endif
            }
            sit++;
        }
        //post_deletion
        if (!d_m)
        {
            xptr del_node = (*it3).cells[0].get_safenode();
            delete_node(del_node, &delete_node_context);
        }
next_replacement:;
    }
    while (it3 != arg4seq.begin());

    if (ins_swiz != NULL)
    {
        delete ins_swiz;
    }
#ifdef SE_ENABLE_FTSEARCH
    execute_modifications();
#endif
#ifdef SE_ENABLE_TRIGGERS
    apply_per_statement_triggers(NULL, false, NULL, false, TRIGGER_AFTER, TRIGGER_REPLACE_EVENT);
#endif
}
