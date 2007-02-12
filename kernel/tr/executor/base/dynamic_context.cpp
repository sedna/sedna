/*
 * File:  dynamic_context.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

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
        case pt_not_defined	: break;
        case pt_tuple		: delete t; break;
        case pt_seq			: delete s; break;
        case pt_lazy_simple	: delete svc; break;
        case pt_lazy_complex: delete cvc; break;
        default				: throw USER_EXCEPTION2(SE1003, "Unexpected case in producer::~producer");
    }
}


global_producer::~global_producer() { delete op; op = NULL; }
void global_producer::open() { op->open(); }
void global_producer::close() { ((PPIterator*)op)->close(); }



/*******************************************************************************
 * Static context
 ******************************************************************************/
static_context::static_context()
{
	def_ns.push_back(NULL);
	xml_ns* tmp=xml_ns::init(NULL,"xml",false);
	insc_ns["xml"].push_back(tmp);
	ns_lib[str_pair("","xml")]=tmp;
	tmp=xml_ns::init("http://www.w3.org/2001/XMLSchema","xs",false);
	insc_ns["xs"].push_back(tmp);
	ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;
	tmp= xml_ns::init("http://www.w3.org/2001/XMLSchema-instance","xsi",false);
	insc_ns["xsi"].push_back(tmp);
	ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;
	tmp=xml_ns::init("http://www.w3.org/2005/04/xpath-functions","fn",false);
	insc_ns["fn"].push_back(tmp);
	ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;
	tmp=xml_ns::init("http://www.w3.org/2005/04/xpath-datatypes","xdt",false);
	insc_ns["xdt"].push_back(tmp);
	ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;
	tmp=xml_ns::init("http://www.w3.org/2005/04/xquery-local-functions","local",false);
	insc_ns["local"].push_back(tmp);
	ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;


    boundary_space = xq_boundary_space_strip;
    default_collation_uri = NULL;
	base_uri = NULL;
	preserve_type = false;
    ordering_mode = xq_ordering_mode_ordered;
    empty_order = xq_empty_order_least;
	cn_preserve = false;
	cn_inherit = false;
    output_indent = se_output_indent_yes;

    /////////////////////////////////////////////////////////////////////////
    /// Set codepoint collation as the default one.
    /// DO NOT call static_context::set_default_collation() here - 
    /// it is too complex to be called from constructor.
    const char* codepoint_collation_uri = "http://www.w3.org/2005/xpath-functions/collation/codepoint";
    default_collation_uri = new char[strlen(codepoint_collation_uri) + 1];
    strcpy(default_collation_uri, codepoint_collation_uri);
    default_collation_handler = dynamic_context::collation_manager.get_collation_handler(codepoint_collation_uri);
    /////////////////////////////////////////////////////////////////////////
}

static_context::~static_context() 
{
    std::vector<xptr>::iterator cit=temp_docs.begin();
    while (cit!=temp_docs.end())
    {
        xptr nd=*cit;
        CHECKP(nd);
        GETSCHEMENODEX(nd)->delete_scheme_node();
        //nid_delete(nd);		
        ++cit;
    }
    temp_docs.clear();


    ns_map ::iterator it=ns_lib.begin();
    while (it !=ns_lib.end())
    {
        if  (it->second!=NULL)
        {
            xml_ns::delete_namespace_node(it->second);
        }
        it++;
    }
    insc_ns.clear();
    ns_lib.clear();
    def_ns.clear();


	//if (def_ns!=NULL)xml_ns::
	//NEED DELETION
	if (base_uri != NULL)
	{
	    delete base_uri;
	    base_uri = NULL;
	}
	if (default_collation_uri != NULL)
	{
	    delete default_collation_uri;
	    default_collation_uri = NULL;
	}
}

xml_ns* static_context::get_ns_pair(const char* prefix,const char* uri)
{
	const char* pref=(prefix==NULL)?"":prefix;
	ns_map::iterator it=ns_lib.find(str_pair(uri,pref));
	if (it!=ns_lib.end())
		return it->second;
	else
	{
		const char* ur=(my_strcmp(uri,"http://www.w3.org/XML/1998/namespace")==0)?NULL:uri;
		xml_ns* res=xml_ns::init(ur,prefix,false);
		ns_lib[str_pair(uri,pref)]=res;
		return res;	 
	}
}

xml_ns* static_context::add_to_context(const char* prefix,const char* uri)
{
	xml_ns * res=get_ns_pair(prefix,uri);
	if (prefix==NULL)
		def_ns.push_back(res);
	else
	{
		inscmap ::iterator it=insc_ns.find(std::string(prefix));
		if (it!=insc_ns.end())
			it->second.push_back(res);
		else
			insc_ns[std::string(prefix)].push_back(res);
	}
	return res;
}
void static_context::remove_from_context(const char* prefix)
{
	if (prefix==NULL&&def_ns.size()>0)
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

char * static_context::get_uri_by_prefix(const char* _prefix,t_item type) const
{
    std::string prefix(_prefix);
	char* uri;
	if (prefix.size()==0)
	{
		if (type!=attribute)
		{
			xml_ns* ns=	def_ns.back();
			uri=(ns==NULL)?NULL:ns->uri;
		}
		else uri=NULL;
	}
	else
	{
		inscmap::const_iterator it=insc_ns.find(prefix);
		if (it!=insc_ns.end()&& it->second.size()>0)
			uri=it->second.back()->uri;
		else
			throw USER_EXCEPTION(XPST0008);
	}
	return uri;
}

xml_ns* static_context::get_xmlns_by_prefix(const char *_prefix, int count)
{
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
			throw USER_EXCEPTION(XQDY0074);
	}
}

void static_context::set_base_uri(const char* _base_uri_)
{
    ///////////////////////////////////////////////////////////////////////
    /// Check constraints on URILiteral.
    bool valid;
    Uri::Information nfo;
    Uri::check_constraints(_base_uri_, &valid, &nfo);
    if (!valid) throw USER_EXCEPTION2(XQST0046, "Prolog base-uri property contains invalid URI.");
    ///////////////////////////////////////////////////////////////////////
    
    ///////////////////////////////////////////////////////////////////////
    /// Delete old value if any.
    if(base_uri != NULL) delete base_uri;
    ///////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////
    /// Normalize URI if needed and create new value.
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
    ///////////////////////////////////////////////////////////////////////
}

void static_context::set_default_collation_uri(const char* _default_collation_uri_)
{
    tuple_cell tc;
    
    ///////////////////////////////////////////////////////////////////////
    /// Check constraints on URILiteral.
    bool valid;
    Uri::Information nfo;
    Uri::check_constraints(_default_collation_uri_, &valid, &nfo);
    if (!valid) throw USER_EXCEPTION2(XQST0046, "Prolog default-collation property contains invalid URI.");
    ///////////////////////////////////////////////////////////////////////
    
    if(nfo.type == Uri::UT_RELATIVE && base_uri == NULL) 
        throw USER_EXCEPTION2(XQST0038, "Unknown collation in prolog (it could not be relative while base-uri is not defined).");
    
    const char* normalized_value = _default_collation_uri_;

    ///////////////////////////////////////////////////////////////////////
    /// Normalize URI if needed.
    if(!nfo.normalized) 
    {
        stmt_str_buf result;
        collapse_string_normalization(_default_collation_uri_, result);
        tc = tuple_cell::make_sure_light_atomic(result.get_tuple_cell());
        normalized_value = tc.get_str_mem();
    }
    ///////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////
    /// And try to resolve it over base-uri property if it is relative.
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
           if(e.get_code() == FORG0009) throw USER_EXCEPTION2(XQST0038, "Unknown collation in prolog (possibly, base-uri contains relative URI).");
           throw;
       }
    }
    ///////////////////////////////////////////////////////////////////////
    
    default_collation_handler = dynamic_context::collation_manager.get_collation_handler(normalized_value);
    if(default_collation_handler == NULL) throw USER_EXCEPTION2(XQST0038, "Unknown collation in prolog (statically unknown collation).");            

    if (default_collation_uri != NULL) delete default_collation_uri;
    default_collation_uri = new char[strlen(normalized_value) + 1];
    strcpy(default_collation_uri, normalized_value);
}

/// Collarions are used primarily in F&O Spec. so we raise 
/// FOCH0002 in this method. XQTS0076 is equivalent in XQuery Spec. 
/// Use try-catch to change FOCH0002 to XQST0076 or other.
CollationHandler* static_context::get_collation(const char *uri)
{
    if (!uri) return get_default_collation();
    
    ///////////////////////////////////////////////////////////////////////
    /// Check constraints on the given URI.
    bool valid;
    Uri::Information nfo;
    Uri::check_constraints(uri, &valid, &nfo);
    if (!valid) throw USER_EXCEPTION2(FOCH0002, "Given URI is not valid.");
    ///////////////////////////////////////////////////////////////////////
    
    if(nfo.type == Uri::UT_RELATIVE && base_uri == NULL) 
        throw USER_EXCEPTION2(FOCH0002, "Given URI is relative and base-uri property is not defined.");

    tuple_cell tc;
    const char *normalized_value = uri;
    
    ///////////////////////////////////////////////////////////////////////
    /// Normalize URI if needed.
    if(!nfo.normalized) 
    {
        stmt_str_buf result;
        collapse_string_normalization(normalized_value, result);
        tc = tuple_cell::make_sure_light_atomic(result.get_tuple_cell());
        normalized_value = tc.get_str_mem();
    }
    ///////////////////////////////////////////////////////////////////////
    
    ///////////////////////////////////////////////////////////////////////
    /// And try to resolve it over base-uri property if it is relative.
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
           if(e.get_code() == FORG0009) throw USER_EXCEPTION2(FOCH0002, "Given URI is relative and base-uri contains relative URI.");
           throw;
       }
    }
    ///////////////////////////////////////////////////////////////////////
    
    CollationHandler *ch = dynamic_context::collation_manager.get_collation_handler(normalized_value);
    if (!ch)
        throw USER_EXCEPTION(FOCH0002);

    return ch;
}










/*******************************************************************************
 * Dynamic context
 ******************************************************************************/

global_variable_context dynamic_context::glb_var_cxt;
function_context dynamic_context::funct_cxt;
static_context **dynamic_context::st_cxts = NULL;
int dynamic_context::st_cxts_num = 0;
int dynamic_context::st_cxts_pos = 0;

static_context *dynamic_context::unmanaged_st_cxt = NULL;

CollationManager dynamic_context::collation_manager;
se_output_method dynamic_context::output_method = se_output_method_xml;

XMLDateTime dynamic_context::current_datetime;
XMLDateTime dynamic_context::current_date;
XMLDateTime dynamic_context::current_time;
XMLDateTime dynamic_context::implicit_timezone;
bool dynamic_context::datetime_initialized = false;

StrMatcher dynamic_context::stm;

se_ostream* dynamic_context::m_ostr = NULL;
se_ostream* dynamic_context::m_dostr = NULL;

void dynamic_context::static_set(int _funcs_num_, int _var_decls_num_, int _st_cxts_num_, se_ostream& s)
{
    funct_cxt.set(_funcs_num_);
    glb_var_cxt.set(_var_decls_num_);

    U_ASSERT(_st_cxts_num_ > 0);
    st_cxts_num = _st_cxts_num_;
    st_cxts_pos = 0;
    st_cxts = new static_context*[st_cxts_num];

	stm.reset();//REDO!!!!
	stm.add_str(">","&gt;");
	stm.add_str("<","&lt;");
	stm.add_str("&","&amp;");
	stm.add_str("\"","&quot;", pat_attribute);

    m_ostr = &s;
    // firstly debug ostream is null; it is created if needed
    m_dostr = NULL;
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

    if (m_dostr)
    {
        delete m_dostr;
        m_dostr = NULL;
    }
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

se_ostream& dynamic_context::dostr() 
{ 
    if (!m_dostr)
        m_dostr = m_ostr->get_debug_ostream();

    return *m_dostr;
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

int __cpp_is_stack_trace_debug()
{
    return dynamic_context::stack_trace_debug;
}

// session options Scheme part
extern "C" {

int is_stack_trace_debug()
{
    return __cpp_is_stack_trace_debug();
}

} // end of extern "C"

