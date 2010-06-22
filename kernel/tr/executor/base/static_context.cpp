/*
 * File:  static_context.cpp
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/executor/base/static_context.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xs_helper.h"

CollationManager static_context::collation_manager;

struct predef_nsp
{
    const char *prefix;
    const char *uri;
};

static predef_nsp predef_nsps[] =
{
    {"xml", "http://www.w3.org/XML/1998/namespace"},
    {"xs",  "http://www.w3.org/2001/XMLSchema"},
    {"xsi", "http://www.w3.org/2001/XMLSchema-instance"},
    {"fn", "http://www.w3.org/2005/xpath-functions"},
    {"local", "http://www.w3.org/2005/xquery-local-functions"},
    {SEDNA_NAMESPACE_PREFIX, SEDNA_NAMESPACE_URI},
    {}
};

static_context::static_context()
{
    predef_nsp *tmp = &predef_nsps[0];

    /* Initialize default namespace stack */
    def_ns.push_back(NULL_XMLNS);

    /* Initialize predefined namespaces */
    while (tmp->prefix != NULL)
    {
        predefined_ns.insert(xmlns_touch(tmp->uri,tmp->prefix));
        tmp++;
    }

    /* Initialize default values for prolog defined values */
    prolog_set_fields = 0;
    boundary_space = xq_boundary_space_strip;
    default_collation_uri = NULL;
    base_uri = NULL;
    preserve_type = false;
    ordering_mode = xq_ordering_mode_ordered;
    empty_order = xq_empty_order_least;
    cn_preserve = false;
    cn_inherit = false;

    sp.output_indent = se_output_indent_yes;
    sp.output_method = se_output_method_xml;

    /*
     * Set codepoint collation as the default one.
     * static_context::set_default_collation() is too complex to be called from constructor.
     * Probably it's better to make default value NULL?
     */
    const char* codepoint_collation_uri = "http://www.w3.org/2005/xpath-functions/collation/codepoint";
    default_collation_uri = new char[strlen(codepoint_collation_uri) + 1];
    strcpy(default_collation_uri, codepoint_collation_uri);
    default_collation_handler = collation_manager.get_collation_handler(codepoint_collation_uri);
}

static_context::~static_context()
{
    if (base_uri != NULL)
    {
        delete[] base_uri;
        base_uri = NULL;
    }

    if (default_collation_uri != NULL)
    {
        delete[] default_collation_uri;
        default_collation_uri = NULL;
    }
}

void static_context::set_base_uri(const char* _base_uri_)
{
    /* Check constraints on URILiteral. */
    bool valid;
    Uri::Information nfo;
    Uri::check_constraints(_base_uri_, &valid, &nfo);
    if (!valid) throw XQUERY_EXCEPTION2(XQST0046, "Prolog base-uri property contains invalid URI.");

    /* Delete old value if any. */
    if(base_uri != NULL)
    {
        delete base_uri;
        base_uri = NULL;
    }

    /* If provided URI is relative considering this property as undefined */
    if(nfo.type == Uri::UT_RELATIVE) return;

    /* Normalize URI if needed and create new value. */
    if(!nfo.normalized)
    {
        stmt_str_buf result;
        collapse_string_normalization(_base_uri_, result);
        tuple_cell tc = result.get_tuple_cell();
        tc = tuple_cell::make_sure_light_atomic(tc);
        base_uri = new char[tc.get_strlen() + 1];
        strcpy(base_uri, tc.get_str_mem());
    }
    else
    {
        base_uri = new char[strlen(_base_uri_) + 1];
        strcpy(base_uri, _base_uri_);
    }
    set_field_flag(SC_BASE_URI);
}

void static_context::set_default_collation_uri(const char* _default_collation_uri_)
{
    tuple_cell tc;

    /* Check constraints on URILiteral. */
    bool valid;
    Uri::Information nfo;
    Uri::check_constraints(_default_collation_uri_, &valid, &nfo);
    if (!valid) throw XQUERY_EXCEPTION2(XQST0046, "Prolog default-collation property contains invalid URI.");

    if (nfo.type == Uri::UT_RELATIVE && base_uri == NULL)
        throw XQUERY_EXCEPTION2(XQST0038, "Unknown collation in prolog (it could not be relative while base-uri is not defined).");

    const char* normalized_value = _default_collation_uri_;

    /* Normalize URI if needed. */
    if(!nfo.normalized)
    {
        stmt_str_buf result;
        collapse_string_normalization(_default_collation_uri_, result);
        tc = tuple_cell::make_sure_light_atomic(result.get_tuple_cell());
        normalized_value = tc.get_str_mem();
    }

    /* And try to resolve it over base-uri property if it is relative. */
    if(nfo.type == Uri::UT_RELATIVE)
    {
       try
       {
           stmt_str_buf result;
           if(!Uri::resolve(normalized_value, base_uri, result))
               throw USER_EXCEPTION2(SE1003, "Unexpected URI resolving error in set_default_collation.");
           tc = tuple_cell::make_sure_light_atomic(result.get_tuple_cell());
           normalized_value = tc.get_str_mem();
       }
       catch(SednaUserException &e)
       {
           if(e.get_code() == FORG0009) throw XQUERY_EXCEPTION2(XQST0038, "Unknown collation in prolog (possibly, base-uri contains relative URI).");
           throw;
       }
    }

    default_collation_handler = collation_manager.get_collation_handler(normalized_value);
    if(default_collation_handler == NULL) throw XQUERY_EXCEPTION2(XQST0038, "Unknown collation in prolog (statically unknown collation).");

    if (default_collation_uri != NULL) delete[] default_collation_uri;
    default_collation_uri = new char[strlen(normalized_value) + 1];
    strcpy(default_collation_uri, normalized_value);
    set_field_flag(SC_DEFAULT_COLLATION_URI);
}

int static_context::get_collation(const char *uri, /* out */ CollationHandler** handler)
{
    if ( !uri )
    {
        *handler = get_default_collation();
        return 0;
    }

    /* 1. Check constraints on the given URI. */
    bool valid;
    Uri::Information nfo;
    Uri::check_constraints(uri, &valid, &nfo);
    if ( !valid ) return COLLATION_INVALID_URI;

    if(nfo.type == Uri::UT_RELATIVE && base_uri == NULL)
        return COLLATION_RESOLVE_ERR;

    tuple_cell tc;
    const char *normalized_value = uri;

    /* 2. Normalize URI if needed. */
    if(!nfo.normalized)
    {
        stmt_str_buf result;
        collapse_string_normalization(normalized_value, result);
        tc = tuple_cell::make_sure_light_atomic(result.get_tuple_cell());
        normalized_value = tc.get_str_mem();
    }

    /* 3. And try to resolve it over base-uri property if it is relative. */
    if(nfo.type == Uri::UT_RELATIVE)
    {
       try
       {
           stmt_str_buf result;
           if(!Uri::resolve(normalized_value, base_uri, result))
               throw USER_EXCEPTION2(SE1003, "Unexpected URI resolving error in get_collation.");
           tc = tuple_cell::make_sure_light_atomic(result.get_tuple_cell());
           normalized_value = tc.get_str_mem();
       }
       catch(SednaUserException &e)
       {
           if(e.get_code() == FORG0009) return COLLATION_RESOLVE_ERR;
           throw;
       }
    }

    /// 4. Get handler from the resolved and normalized value.
    *handler = collation_manager.get_collation_handler(normalized_value);
    if (!*handler) return COLLATION_MISS;
    return 0;
}

xmlns_ptr static_context::get_predef_nsp(const char *prefix)
{
    predef_nsp *tmp = &predef_nsps[0];

    while (tmp->prefix != NULL)
    {
        if (!strcmp(tmp->prefix, prefix))
            break;

        tmp++;
    }

    if (tmp->uri) // found
        return xmlns_touch(tmp->prefix, tmp->uri);

    return NULL_XMLNS;
}
