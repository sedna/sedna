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
#include "strings.h"
#include "utf8.h"

typedef std::pair<std::string,std::string> str_pair;
typedef std::map< str_pair, xml_ns*> ns_map;
typedef std::map<std::string,std::vector<xml_ns*> > inscmap;


enum se_output_method {se_output_method_xml};

enum se_output_indent {se_output_indent_yes, se_output_indent_no};

enum xq_boundary_space {xq_boundary_space_strip, xq_boundary_space_preserve};

enum xq_ordering_mode {xq_ordering_mode_ordered, xq_ordering_mode_unordered};

enum xq_empty_order {xq_empty_order_greatest, xq_empty_order_least};

struct static_context
{
    /// Prolog Declarations
    /// ~~~~~~~~~~~~~~~~~~~
    /// Boundary-space Declaration
    xq_boundary_space boundary_space;
    /// Default Collation Declaration
    char* default_collation_uri;
    /// Base URI Declaration
    char* base_uri;
    /// Construction Declaration ('preserve'=true, 'strip'=false)
    bool preserve_type;
    /// Ordering Mode Declaration
    xq_ordering_mode ordering_mode;
    /// Empty Order Declaration
    xq_empty_order empty_order;
    /// Copy-Namespaces Declaration
    bool cn_preserve;
    bool cn_inherit;
    /// Namespace Declaration (it is not a field; it's added by calling add_to_context)
    /// Default Namespace Declaration for element and function (they are not fields; they are added by calling add_to_context)
    /// Variable Declaration is handled some other way
    /// Function Declaration is handled some other way
    /// Option Declaration (we have the following options)
    se_output_method output_method;
    se_output_indent output_indent;


    /// stores pointer to default collation handler already resolved using default collation uri and base uri
    CollationHandler *default_collation_handler;
	CollationManager collation_manager;


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


    static_context();
    ~static_context() ;
    void _init_context();
    void _release_resources();
    void clear_context();

    xml_ns* get_ns_pair(const char* prefix,const char* uri);
    inline xml_ns* get_ns_pair(std::string& prefix,std::string& uri)
    {
    	return get_ns_pair((prefix.size()>0)?prefix.c_str():NULL,uri.c_str());
    }
    xml_ns* add_to_context(const char* prefix,const char* uri);


    void add_char_mapping(const char* str, const char* rep_str,int pc =-1);
    void remove_from_context(const char* prefix);
    inline void  remove_from_context(xml_ns* ns)
    { 
        remove_from_context(ns->prefix);
    }
    char * get_uri_by_prefix(const char* _prefix, t_item type) const;
    xml_ns* get_xmlns_by_prefix(const char* _prefix, int count = -1);

    void set_datetime();
    void set_base_uri(const char* _base_uri_);
    void set_default_collation_uri(const char* _default_collation_uri_);

    /// resolves uri and returns collation handler
    /// if uri is NULL, returns default collation
    CollationHandler* get_collation(const char *uri);
    CollationHandler* get_default_collation() { return default_collation_handler; }
};

#endif

