/*
 * File:  PPStaticContext.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSTATCONT_H
#define _PPSTATCONT_H

#include <map>
#include "sedna.h"
#include "schema.h"
#include "XPath.h"

typedef std::pair<std::string,std::string> str_pair;
typedef std::map< str_pair, xml_ns*> ns_map;
typedef std::map<std::string,std::vector<xml_ns*> > inscmap;


enum se_output_method {se_output_method_xml};

enum se_output_indent {se_output_indent_yes, se_output_indent_no};

enum xq_boundary_space {xq_boundary_space_strip, xq_boundary_space_preserve};

struct static_context
{
    se_output_method output_method;
    se_output_indent output_indent;
    xq_boundary_space boundary_space;

	std::vector<xml_ns*> def_ns;
	inscmap insc_ns;
	ns_map ns_lib;
    static_context();
	void clear_context();
    ~static_context() 
	{
		//if (def_ns!=NULL)xml_ns::
		//NEED DELETION
	}
	xml_ns* get_ns_pair(const char* prefix,const char* uri);
	inline xml_ns* get_ns_pair(std::string& prefix,std::string& uri)
	{
		return get_ns_pair((prefix.size()>0)?prefix.c_str():NULL,uri.c_str());
	}
	xml_ns* add_to_context(const char* prefix,const char* uri);
	void remove_from_context(const char* prefix);
	inline void  remove_from_context(xml_ns* ns)
	{ remove_from_context(ns->prefix); }
	char * get_uri_by_prefix(const NCName& _prefix, t_item type) const;
	xml_ns* static_context::get_xmlns_by_prefix(const NCName& _prefix);
	

};
#endif

