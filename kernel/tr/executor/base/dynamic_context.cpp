/*
 * File:  dynamic_context.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
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


global_producer::~global_producer() { delete op; op = NULL; delete cxt; cxt = NULL; }
void global_producer::open() { op->open(); }
void global_producer::close() { ((PPIterator*)op)->close(); }


/*******************************************************************************
 * Static context
 ******************************************************************************/
static_context::static_context()
{
    /* Initialize default namespace stack */
    def_ns.push_back(NULL_XMLNS);
    
    /* Initialize predefined namespaces */
    insc_ns["xml"].  push_back(xmlns_touch("xml", "http://www.w3.org/XML/1998/namespace"));
    insc_ns["xs"].   push_back(xmlns_touch("xs", "http://www.w3.org/2001/XMLSchema"));
    insc_ns["xsi"].  push_back(xmlns_touch("xsi", "http://www.w3.org/2001/XMLSchema-instance"));
    insc_ns["fn"].   push_back(xmlns_touch("fn", "http://www.w3.org/2005/xpath-functions"));
    insc_ns["local"].push_back(xmlns_touch("local", "http://www.w3.org/2005/xquery-local-functions"));
    insc_ns["se"].   push_back(xmlns_touch(SEDNA_NAMESPACE_PREFIX, SEDNA_NAMESPACE_URI));
    
    inscmap::iterator it_end = insc_ns.end();
    for(inscmap::iterator it = insc_ns.begin(); it != it_end; it++)
    {
        xmlns_ptr tmp = it->second.front();
        ns_lib[str_pair(tmp->uri,tmp->prefix)] = tmp;
        predefined_ns.insert(tmp);
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
    output_indent = se_output_indent_yes;

    /* 
     * Set codepoint collation as the default one.
     * static_context::set_default_collation() is too complex to be called from constructor.
     * Probably it's better to make default value NULL?
     */
    const char* codepoint_collation_uri = "http://www.w3.org/2005/xpath-functions/collation/codepoint";
    default_collation_uri = se_new char[strlen(codepoint_collation_uri) + 1];
    strcpy(default_collation_uri, codepoint_collation_uri);
    default_collation_handler = dynamic_context::collation_manager.get_collation_handler(codepoint_collation_uri);
}

static_context::~static_context()
{
    std::vector<xptr>::iterator cit=temp_docs.begin();
    while (cit!=temp_docs.end())
    {
        xptr nd=*cit;
        CHECKP(nd);
        GETSCHEMENODEX(nd)->drop();
        ++cit;
    }
    temp_docs.clear();

    insc_ns.clear();
    ns_lib.clear();
    def_ns.clear();

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

/* 
 * Seems this fucntion should be private, in most cases you want to
 * use get_xmlns_by_prefix() instead. 
 */
xmlns_ptr static_context::get_ns_pair(const char* prefix, const char* uri)
{
    U_ASSERT(prefix);
    
    ns_map::iterator it = ns_lib.find(str_pair(uri,prefix));
    if (it != ns_lib.end())
        return it->second;
    else
    {
        xmlns_ptr res = xmlns_touch(prefix,uri);
        ns_lib[str_pair(uri,prefix)] = res;
        return res;
    }
}

xmlns_ptr static_context::add_to_context(const char* prefix,const char* uri)
{
    U_ASSERT(prefix);
    
    xmlns_ptr res = get_ns_pair(prefix,uri);
    if (strcmp("", prefix) == 0)
    {
        def_ns.push_back(res);
        set_field_flag(SC_DEFAULT_NAMESPACE);
    }
    else
    {
        inscmap::iterator it = insc_ns.find(std::string(prefix));
        if (it!=insc_ns.end())
            it->second.push_back(res);
        else
            insc_ns[std::string(prefix)].push_back(res);
        set_field_flag(SC_NAMESPACE);
    }
    return res;
}

void static_context::remove_from_context(const char* prefix)
{
    U_ASSERT(prefix);    

    if (strcmp(prefix, "") == 0 && def_ns.size()>0)
    {
        def_ns.pop_back();
    }
    else
    {
        inscmap ::iterator it=insc_ns.find(std::string(prefix));
        if (it!=insc_ns.end()&& it->second.size()>0)
            it->second.pop_back();
        else
            throw SYSTEM_EXCEPTION("Static context Error");
    }
}


xmlns_ptr static_context::get_xmlns_by_prefix(const char *_prefix, int count)
{
    U_ASSERT(_prefix);    

    if (count < 0) count = strlen(_prefix);
    std::string prefix(_prefix, count);
    if (prefix.size()==0)
    {
        return def_ns.back();
    }
    else
    {
        inscmap::const_iterator it=insc_ns.find(prefix);
        if (it!=insc_ns.end()&& it->second.size()>0)
            return it->second.back();
        else
            throw XQUERY_EXCEPTION(XQDY0074);
    }
}

/* Returns all explicitly defined (except predefined) namespaces */
std::vector<xmlns_ptr> static_context::get_explicit_namespaces()
{
    std::vector<xmlns_ptr> res;
    inscmap::iterator it_end = insc_ns.end();
    for(inscmap::iterator it = insc_ns.begin(); it != it_end; it++)
    {
        xmlns_ptr tmp = it->second.front();
        std::set<xmlns_ptr>::iterator predefined_it = predefined_ns.find(tmp);
        if(predefined_it == predefined_ns.end())
        {
            res.push_back(tmp);
        }
    }
    return res;

}

/* Returns current default namespaces or NULL_XMLNS if there's no one */
xmlns_ptr static_context::get_default_namespace()
{
    if(def_ns.empty()) return NULL_XMLNS;
    else return def_ns.back();
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
        base_uri = se_new char[tc.get_strlen() + 1];
        strcpy(base_uri, tc.get_str_mem());
    }
    else
    {
        base_uri = se_new char[strlen(_base_uri_) + 1];
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

    if(nfo.type == Uri::UT_RELATIVE && base_uri == NULL)
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

    default_collation_handler = dynamic_context::collation_manager.get_collation_handler(normalized_value);
    if(default_collation_handler == NULL) throw XQUERY_EXCEPTION2(XQST0038, "Unknown collation in prolog (statically unknown collation).");

    if (default_collation_uri != NULL) delete [] default_collation_uri;
    default_collation_uri = se_new char[strlen(normalized_value) + 1];
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
    *handler = dynamic_context::collation_manager.get_collation_handler(normalized_value);
    if (!*handler) return COLLATION_MISS;
    return 0;
}




/*******************************************************************************
 * Function declaration
 ******************************************************************************/

/// !!! Destructor must be here, not in 'h' file (I.S.)
function_declaration::~function_declaration()
{
    delete [] args;
    delete op;
}



/*******************************************************************************
 * Dynamic context
 ******************************************************************************/

global_variable_context dynamic_context::glb_var_cxt;
function_context dynamic_context::funct_cxt;
static_context **dynamic_context::st_cxts = NULL;
int dynamic_context::st_cxts_num = 0;
int dynamic_context::st_cxts_pos = 0;

std::vector<static_context *> dynamic_context::unmanaged_st_cxts;

CollationManager dynamic_context::collation_manager;
se_output_method dynamic_context::output_method = se_output_method_xml;

XMLDateTime dynamic_context::current_datetime;
XMLDateTime dynamic_context::current_date;
XMLDateTime dynamic_context::current_time;
XMLDateTime dynamic_context::implicit_timezone;
bool dynamic_context::datetime_initialized = false;

StrMatcher dynamic_context::stm;

void dynamic_context::static_set(int _funcs_num_, int _var_decls_num_, int _st_cxts_num_)
{
    funct_cxt.set(_funcs_num_);
    glb_var_cxt.set(_var_decls_num_);

    U_ASSERT(_st_cxts_num_ > 0);
    st_cxts_num = _st_cxts_num_;
    st_cxts_pos = 0;
    st_cxts = se_new static_context*[st_cxts_num];

    stm.reset();
    stm.add_str(">","&gt;");
    stm.add_str("<","&lt;");
    stm.add_str("&","&amp;");
    stm.add_str("\"","&quot;", pat_attribute);
}

void dynamic_context::static_clear()
{
    datetime_initialized = false;
    output_method = se_output_method_xml;

    glb_var_cxt.clear();
    funct_cxt.clear();

    for (st_cxts_pos = 0; st_cxts_pos < st_cxts_num; st_cxts_pos++)
    {
        delete st_cxts[st_cxts_pos];
        st_cxts[st_cxts_pos] = NULL;
    }
    st_cxts_num = 0;
    st_cxts_pos = 0;
    delete [] st_cxts;
    st_cxts = NULL;

    for (size_t i = 0; i < unmanaged_st_cxts.size(); i++)
        delete unmanaged_st_cxts[i];

    unmanaged_st_cxts.clear();
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


int dynamic_context::stack_trace_debug;

void dynamic_context::set_session_option(se_session_option type, const void* s, int n)
{
    switch (type)
    {
        case se_debug_mode:
            stack_trace_debug = *(int*)s;
            break;
        default:
            throw USER_EXCEPTION2(SE4617, "Unknown option");
    }
}

void dynamic_context::reset_session_options()
{
    stack_trace_debug = SEDNA_DEBUG_OFF;
}


