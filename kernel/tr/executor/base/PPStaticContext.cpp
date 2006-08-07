/*
 * File:  PPStaticContext.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPStaticContext.h"

static_context::static_context()
{
        output_method = se_output_method_xml;
        output_indent = se_output_indent_yes;
        boundary_space = xq_boundary_space_strip;
        empty_order = xq_empty_order_least;

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
	if (prefix==NULL &&def_ns.size()>0)
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

char * static_context::get_uri_by_prefix(const NCName& _prefix,t_item type) const
{
    std::string prefix(_prefix.n);
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
xml_ns* static_context::get_xmlns_by_prefix(const NCName& _prefix)
{
    std::string prefix(_prefix.n);
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
			throw USER_EXCEPTION(XPST0008);
	}
}
void static_context::clear_context()
{
	    output_method = se_output_method_xml;
        output_indent = se_output_indent_yes;
        boundary_space = xq_boundary_space_strip;
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
}
