/*
 * File:  PPQueryRoot.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/executor/root/PPQueryRoot.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/crmutils/crmutils.h"
#include "tr/crmutils/serialization.h"
#include "tr/locks/locks.h"
#include "tr/tr_globals.h"

PPQueryRoot::PPQueryRoot(dynamic_context *_cxt_,
                         PPOpIn _child_) :       PPQueryEssence("PPQueryRoot"),
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

void PPQueryRoot::do_open()
{
    local_lock_mrg->lock(lm_s);
    first = true;
    cxt->global_variables_open();
    child.op->open();

/*
    if (print_mode == sxml)
    {
        cxt->add_char_mapping("<","<",-1);
        cxt->add_char_mapping(">",">",-1);
        cxt->add_char_mapping("&","&",-1);
        cxt->add_char_mapping("\"","\\\"",-1);
        cxt->add_char_mapping("\\","\\\\",-1);
    }
*/

    /* Set serialization options for client */
//    tr_globals::client->get_serializer()-> (cxt->get_static_context()->get_serialization_params());
}

void PPQueryRoot::do_close()
{
    child.op->close();
    cxt->global_variables_close();
}

void PPQueryRoot::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

bool PPQueryRoot::next()
{
    if(executor_globals::profiler_mode)
    {
        U_ASSERT(info.profile.get() != NULL);
        info.profile->calls++;
        u_ftime(&current1);
    }
    if(1 == tr_globals::debug_mode)
    {
        U_ASSERT(info.profile.get() != NULL);
        if(!executor_globals::profiler_mode) info.profile->calls++;
        executor_globals::pp_stack.push_back(info);
    }

    bool result = PPQueryRoot::do_next();

    if(1 == tr_globals::debug_mode)
        executor_globals::pp_stack.pop_back();
    if(executor_globals::profiler_mode)
    {
        u_ftime(&current2);
        info.profile->time = current2 - current1;
    }

    return result;
}

bool PPQueryRoot::do_next()
{
    child.op->next(data);

    if (data.is_eos()) return false;

    if(data.cells_number != 1)
        throw XQUERY_EXCEPTION2(SE1003, "Result of the query contains nested sequences");

    tuple_cell tc = data.cells[0];

    t_item nt = element;   /* nt will be ignored if tuple cell contains atomic value */
    xmlscm_type st = 0;    /* by default there is no type (anyType) */
    bool is_node;
    str_counted_ptr uri;   /* for document and attribute nodes */

//    if(first)
//    {
        /* Can't initialize these variables in open() since PPExplain
         * turns off otput after open called.
         */
//        output_stream = tr_globals::client->get_se_ostream();
//        print_mode    = tr_globals::client->get_result_type();
//    }

    if((is_node = tc.is_node())) {
        /* Determine node type */
        xptr node = tc.get_node();
        CHECKP(node);
        nt = getNodeType(node);
        st = getScmType(node);

        if (nt == attribute)  {
            /* Retrieve attribute namespace to return it to the client */
            tuple_cell tc = se_node_namespace_uri(node);
            if ( !tc.is_eos() ) {
               uri = tuple_cell::make_sure_light_atomic(tc).get_str_ptr();
            } else {
              // BUG
            }
        }
        else if(nt == document) {
            /* Retrieve document URI to return it to the client */
            tuple_cell tc = dm_document_uri(node);
            if ( !tc.is_eos() ) {
               tc = tuple_cell::make_sure_light_atomic(tc);
               uri = tc.get_str_ptr();
            }
        }
    }
    else {
        /* Determine atomic type */
        st = tc.get_atomic_type();
    }

    tr_globals::client->begin_item(!is_node, st, nt, uri.get());

    Serializer * serializer = tr_globals::client->get_serializer();

    if (first) {
        serializer->initialize();
    }

/*
    if (!first && !tr_globals::client->supports_serialization()) {
        // This is needed for backward compatibility with old ( < 4 ) versions of protocol
        (*output_stream) << " ";
    }
*/

    serializer->serialize(data);

    while (!cxt->tmp_sequence.empty()) { cxt->tmp_sequence.pop(); }


/*
    // Clients which based on protocol 4 should support serialization
    if(tr_globals::client->supports_serialization())
    {
//         If client supports serialization we don't have to
//         make indentation, space delimiting and so on
        print_tuple(data, *output_stream, cxt, print_mode, true, false);
    }
    else
    {

        switch (cxt->get_static_context()->get_output_indent())
        {
            case se_output_indent_yes: print_tuple(data, *output_stream, cxt, print_mode, first, true);  break;
            case se_output_indent_no : print_tuple(data, *output_stream, cxt, print_mode, first, false); break;
            default                  : throw USER_EXCEPTION2(SE1003, "Unexpected se_output_indent");
        }
	}
*/

    if (first)
        first = false;

    return true;
}

void PPQueryRoot::do_execute()
{
    while (do_next());
}

/*
 * PPSubQuery
 */

PPSubQuery::PPSubQuery(dynamic_context *_cxt_,
                       PPOpIn _child_) :       PPQueryEssence("PPSubQuery"),
                                               child(_child_),
                                               data(_child_.ts),
                                               cxt(_cxt_)
{
}

PPSubQuery::~PPSubQuery()
{
    delete child.op;
    child.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPSubQuery::do_open()
{
    /*
     * we should proobably save the existent lock mode here since subquery
     * could be run in the middle of a query
     */
    lmode = local_lock_mrg->get_cur_lock_mode();
    local_lock_mrg->lock(lm_s);
    cxt->global_variables_open();
    child.op->open();
}

void PPSubQuery::do_close()
{
    child.op->close();
    cxt->global_variables_close();
    local_lock_mrg->lock(lmode);
}

void PPSubQuery::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

bool PPSubQuery::next(tuple &t)
{
    if (executor_globals::profiler_mode)
    {
        U_ASSERT(info.profile.get() != NULL);
        info.profile->calls++;
        u_ftime(&current1);
    }

    if (1 == tr_globals::debug_mode)
    {
        U_ASSERT(info.profile.get() != NULL);
        if (!executor_globals::profiler_mode) info.profile->calls++;
        executor_globals::pp_stack.push_back(info);
    }

    bool result = PPSubQuery::do_next();

    if (1 == tr_globals::debug_mode)
        executor_globals::pp_stack.pop_back();

    if (executor_globals::profiler_mode)
    {
        u_ftime(&current2);
        info.profile->time = current2 - current1;
    }

    t = data;

    return result;
}

bool PPSubQuery::do_next()
{
    child.op->next(data);

    if (data.is_eos())
        return false;

    if (data.cells_number != 1)
        throw XQUERY_EXCEPTION2(SE1003, "Result of a subquery contains nested sequences");

    return true;
}

void PPSubQuery::do_execute()
{
    U_ASSERT(false);
}
