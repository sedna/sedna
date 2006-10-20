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
#include "str_matcher.h"

typedef std::pair<std::string,std::string> str_pair;
typedef std::map< str_pair, xml_ns*> ns_map;
typedef std::map<std::string,std::vector<xml_ns*> > inscmap;


enum se_output_method {se_output_method_xml};

enum se_output_indent {se_output_indent_yes, se_output_indent_no};

enum xq_boundary_space {xq_boundary_space_strip, xq_boundary_space_preserve};

enum xq_empty_order {xq_empty_order_greatest, xq_empty_order_least};

struct static_context
{
    se_output_method output_method;
    se_output_indent output_indent;
    xq_boundary_space boundary_space;
    xq_empty_order empty_order;
	StrMatcher stm; 
	std::vector<xml_ns*> def_ns;
	std::vector<xptr> temp_docs;
	inscmap insc_ns;
	ns_map ns_lib;

    XMLDateTime current_datetime;
    XMLDateTime current_date;
    XMLDateTime current_time;
    XMLDateTime implicit_timezone;
    bool datetime_initialized;
	bool preserve_type;

    char* base_uri;

    static_context();
	void clear_context();
    ~static_context() 
	{
		//if (def_ns!=NULL)xml_ns::
		//NEED DELETION
		if(base_uri!=NULL)
		{
		    delete base_uri;
		    base_uri = NULL;
		}
	}
	xml_ns* get_ns_pair(const char* prefix,const char* uri);
	inline xml_ns* get_ns_pair(std::string& prefix,std::string& uri)
	{
		return get_ns_pair((prefix.size()>0)?prefix.c_str():NULL,uri.c_str());
	}
	xml_ns* add_to_context(const char* prefix,const char* uri);
	void init_context();
	void set_datetime();	
	void add_char_mapping(const char* str, const char* rep_str,int pc =-1);
	void remove_from_context(const char* prefix);
	inline void  remove_from_context(xml_ns* ns)
	{ remove_from_context(ns->prefix); }
	char * get_uri_by_prefix(const char* _prefix, t_item type) const;
	xml_ns* get_xmlns_by_prefix(const char* _prefix);
	void set_base_uri(const char* _base_uri_);
};
#endif

