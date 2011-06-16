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

    // xml is always in inscope namespaces
    insc_ns["xml"].push_back(xmlns_touch("xml", "http://www.w3.org/XML/1998/namespace"));

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

    while (!tmp_sequence.empty()) {
        delete tmp_sequence.top();
        tmp_sequence.pop();
    }

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

xmlns_ptr dynamic_context::get_xmlns_by_prefix(const char *prefix)
{
    U_ASSERT(prefix);
    xmlns_ptr res;

    if (!strlen(prefix)) {
        return st_cxt->get_default_nsp();
    } else {
        inscmap::const_iterator it = insc_ns.find(prefix);

        if (it != insc_ns.end() && it->second.size() > 0)
            return it->second.back();
        else if ((res = st_cxt->get_predef_nsp(prefix)) != NULL_XMLNS)
            return res;
        else
            throw XQUERY_EXCEPTION(XQDY0074);
    }
}

xmlns_ptr dynamic_context::add_to_context(xmlns_ptr xmlns)
{
    if (!xmlns->has_prefix()) {
        st_cxt->set_default_nsp(xmlns);
    } else {
        insc_ns[std::string(xmlns->get_prefix())].push_back(xmlns);
        st_cxt->set_nsp(xmlns); // QUESTION: WTF???
    }

    return xmlns;
}

void dynamic_context::remove_from_context(const char* prefix)
{
    U_ASSERT(NULL != prefix);

    if (strcmp(prefix, "") == 0) {
        st_cxt->unset_default_nsp();
    } else {
        inscmap::iterator it = insc_ns.find(std::string(prefix));
        if (it != insc_ns.end() && it->second.size() > 0) {
            it->second.pop_back();
        } else {
            throw SYSTEM_EXCEPTION("dynamic context Error");
        }
    }
}

/* Returns all explicitly defined (except predefined) namespaces */
std::vector<xmlns_ptr> dynamic_context::get_explicit_namespaces() const
{
    std::vector<xmlns_ptr> res;

    inscmap::const_iterator it_end = insc_ns.end();
    for (inscmap::const_iterator it = insc_ns.begin(); it != it_end; it++)
    {
        xmlns_ptr tmp = it->second.front();
        if (!st_cxt->is_nsp_predefined(tmp))
        {
            res.push_back(tmp);
        }
    }
    return res;

}
