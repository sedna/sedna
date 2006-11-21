/*
 * File:  PPStaticContext.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "vmm.h"
#include "PPStaticContext.h"

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


	_init_context();
}

static_context::~static_context() 
{
    _release_resources();
}

void static_context::_init_context()
{
    boundary_space = xq_boundary_space_strip;
    default_collation_uri = NULL;
	base_uri = NULL;
	preserve_type = false;
    ordering_mode = xq_ordering_mode_ordered;
    empty_order = xq_empty_order_least;
	cn_preserve = false;
	cn_inherit = false;
    output_method = se_output_method_xml;
    output_indent = se_output_indent_yes;


	datetime_initialized = false;


	stm.reset();//REDO!!!!
	stm.add_str(">","&gt;");
	stm.add_str("<","&lt;");
	stm.add_str("&","&amp;");
	stm.add_str("\"","&quot;", pat_attribute);
}

void static_context::_release_resources()
{
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


void static_context::clear_context()
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
    tmp=xml_ns::init("http://www.w3.org/2003/11/xpath-functions","fn",false);
    insc_ns["fn"].push_back(tmp);
    ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;
    tmp=xml_ns::init("http://www.w3.org/2003/11/xpath-datatypes","xdt",false);
    insc_ns["xdt"].push_back(tmp);
    ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;
    tmp=xml_ns::init("http://www.w3.org/2003/11/xquery-local-functions","local",false);
    insc_ns["local"].push_back(tmp);
    ns_lib[str_pair(tmp->uri,tmp->prefix)]=tmp;


    _release_resources();
	_init_context();
}

void static_context::add_char_mapping(const char* str, const char* rep_str,int pc )
{
	stm.add_str(str,rep_str,pc);
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

void static_context::set_datetime()
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
void static_context::set_base_uri(const char* _base_uri_)
{
    // FIXME: check lexical representation for uri and normalize it
    if(base_uri != NULL) delete base_uri;
    base_uri = new char[strlen(_base_uri_) + 1];
    strcpy(base_uri, _base_uri_);
}

void static_context::set_default_collation_uri(const char* _default_collation_uri_)
{
    // FIXME: check lexical representation for uri and normalize it
    if (default_collation_uri != NULL) delete default_collation_uri;
    default_collation_uri = new char[strlen(_default_collation_uri_) + 1];
    strcpy(default_collation_uri, _default_collation_uri_);
}
