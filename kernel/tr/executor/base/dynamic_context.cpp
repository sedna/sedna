/*
 * File:  dynamic_context.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/base.h"

#include "tr/executor/base/dynamic_context.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/xs_helper.h"


producer::producer() : type(pt_not_defined),
                 s(NULL),
                 op(NULL),
                 svc(NULL),
                 cvc(NULL),
                 tuple_pos(0),
                 t(NULL)
{
}

producer::~producer()
{
    switch (type)
    {
        case pt_not_defined : break;
        case pt_tuple       : delete t; break;
        case pt_seq         : delete s; break;
        case pt_lazy_simple : delete svc; break;
        case pt_lazy_complex: delete cvc; break;
        default             : throw USER_EXCEPTION2(SE1003, "Unexpected case in producer::~producer");
    }
}

void global_producer::open()
{
    op->open();
}

void global_producer::close()
{
    ((PPIterator*)op)->close();
}

/*******************************************************************************
 * Global variable context
 ******************************************************************************/

global_variable_context::~global_variable_context()
{
    // delete global vars operations
    for (std::vector<global_producer>::iterator it = producers.begin();
            it != producers.end(); it++)
        delete it->op;
}

/*******************************************************************************
 * Function context
 ******************************************************************************/

function_context::~function_context()
{
    // delete function operations and arg sequencies
    for (std::vector<function_declaration>::iterator it = fun_decls.begin();
            it != fun_decls.end(); it++)
    {
        delete[] it->args;
        delete it->op;
    }
}

/*******************************************************************************
 * Dynamic context
 ******************************************************************************/

dynamic_context::dynamic_context(static_context *_st_cxt_) : st_cxt(_st_cxt_)
{
    current_var_cxt = NULL;
    curr_var_dsc = curr_gvar_dsc = curr_func_id = 0;
    datetime_initialized = false;
}

dynamic_context::~dynamic_context()
{
    // destroy children-contexts
    for (size_t i = 0; i < child_cxts.size(); i++)
        delete child_cxts[i];

    // drop temporary docs
    std::vector<xptr>::iterator cit=temp_docs.begin();
    while (cit != temp_docs.end())
    {
        xptr nd = *cit;
        CHECKP(nd);
        getSchemaNode(nd)->drop();
        ++cit;
    }

    // destroy all variable contexts
    for (size_t i = 0; i < var_cxts.size(); i++)
        delete var_cxts[i];

    // destroy static context
    delete st_cxt;
}

void dynamic_context::reset_local_vars()
{
    // reuse current context if we haven't used it
    if (!current_var_cxt || current_var_cxt->size != 0)
    {
        current_var_cxt = new variable_context();
        var_cxts.push_back(current_var_cxt);
    }

    curr_var_dsc = 0;
}

void dynamic_context::global_variables_open()
{
    // open children-contexts
    for (size_t i = 0; i < child_cxts.size(); i++)
        child_cxts[i]->global_variables_open();

    glb_var_cxt.open();
}

void dynamic_context::global_variables_close()
{
    glb_var_cxt.close();

    // close children-contexts
    for (size_t i = 0; i < child_cxts.size(); i++)
        child_cxts[i]->global_variables_close();
}

void dynamic_context::set_datetime()
{
    if (!datetime_initialized)
    {
        datetime_initialized = true;
        utm tm = getLocalTime();
        current_datetime = XMLDateTime(tm);
        current_date = XMLDateTime(tm).convertTo(xs_date);
        current_time = XMLDateTime(tm).convertTo(xs_time);
        implicit_timezone = XMLDateTime(tm).getTimezone();
    }
}
