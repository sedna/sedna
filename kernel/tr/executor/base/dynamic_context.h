/*
 * File:  dynamic_context.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DYNAMIC_CONTEXT_H
#define _DYNAMIC_CONTEXT_H

#include "sedna.h"

#include <vector>
#include <list>
#include <map>

#include "SequenceType.h"
#include "str_matcher.h"
#include "utf8.h"


/*******************************************************************************
 * List of classes used
 ******************************************************************************/
class PPIterator;
class PPVarIterator;
class sequence;
class static_context;


/*******************************************************************************
 * Different auxiliary types
 ******************************************************************************/
/// Declaration of variable descriptor and auxilary structures
typedef int var_dsc;	// var descriptor
typedef int var_c_id;	// var consumption id
// every element of the array is the info about consumption of specific cosumer
typedef std::vector<bool> simple_var_consumption;

typedef std::vector<int>  complex_var_consumption;

typedef std::list<int>    free_entries_list;


/// function descriptor
typedef int function_id;


/// namespaces
typedef std::pair<std::string,std::string> str_pair;
typedef std::map< str_pair, xml_ns*> ns_map;
typedef std::map<std::string,std::vector<xml_ns*> > inscmap;


/// query prolog enumerations
enum se_output_method {se_output_method_xml};

enum se_output_indent {se_output_indent_yes, se_output_indent_no};

enum xq_boundary_space {xq_boundary_space_strip, xq_boundary_space_preserve};

enum xq_ordering_mode {xq_ordering_mode_ordered, xq_ordering_mode_unordered};

enum xq_empty_order {xq_empty_order_greatest, xq_empty_order_least};



/*******************************************************************************
 * Variable context
 ******************************************************************************/
// type of a producer
enum producer_type { pt_not_defined,
                     pt_tuple,
                     pt_seq,			// producer is strict operation, so sequence is already built
                     pt_lazy_simple,	// producer is lazy simle
                     pt_lazy_complex,	// producer is lazy complex
                   };

// producer structure
struct producer
{
    producer_type type;				// type of the producer (i.g. lazy or strict)

    sequence *s;					// pointer to completely built sequence with data
    PPVarIterator *op;				// pointer to operation with next(i) method
    simple_var_consumption *svc;
    complex_var_consumption *cvc;
    int tuple_pos;
    tuple *t;

    producer() : type(pt_not_defined), 
                 s(NULL), 
                 op(NULL), 
                 svc(NULL), 
                 cvc(NULL), 
                 tuple_pos(0), 
                 t(NULL) {}
    ~producer()
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
};

// variable context
struct variable_context
{
    int size;				// size of context (number of producers in array)
    producer *producers;	// array of producers

	variable_context(int _size_) : size(_size_) { size > 0 ? producers = new producer[size] : producers = NULL; }
    ~variable_context() { delete [] producers; }
};


/*******************************************************************************
 * Global Variable (declare variable) context
 ******************************************************************************/
struct global_producer
{
    PPVarIterator *op;				// pointer to operation with next(i) method
    complex_var_consumption cvc;
    free_entries_list fel;

    global_producer() : op(NULL) {}
    ~global_producer();
    void open();
    void close();
};

struct global_variable_context
{
    int size;					// size of context (number of producers in array)
    global_producer *producers;	// array of producers

    global_variable_context() : size(0), producers(NULL) {}
    ~global_variable_context() { clear(); }
    void set(int _size_) 
    { 
        size = _size_; 
        producers = size > 0 ? new global_producer[size] : NULL; 
    }
    void clear()
    {
        delete [] producers;
        producers = NULL;
        size = 0;
    }
    void open()
    {
        for (int i = 0; i < size; i++)
            producers[i].open();
    }
    void close()
    {
        for (int i = 0; i < size; i++)
            producers[i].close();
    }
};


/*******************************************************************************
 * Functions context
 ******************************************************************************/
struct function_declaration
{
    sequence_type ret_st;
    int num;
    sequence_type *args;
    PPIterator *op;
    int cxt_size;
    static_context *st_cxt;

    function_declaration() : num(0), args(NULL) {}
    ~function_declaration() { delete [] args; }
};

struct function_context
{
    int size;
    function_declaration *fun_decls;

    function_context() : size(0), fun_decls(NULL) {}
    ~function_context() { clear(); }
    void set(int _size_) 
    { 
        size = _size_; 
        fun_decls = size > 0 ? new function_declaration[size] : NULL; 
    }
    void clear()
    {
        delete [] fun_decls;
        fun_decls = NULL;
        size = 0;
    }
};


/*******************************************************************************
 * Static context
 ******************************************************************************/
class static_context
{
public:
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
    se_output_indent output_indent;

    /// stores pointer to default collation handler already resolved using default collation uri and base uri
    CollationHandler *default_collation_handler;


    std::vector<xml_ns*> def_ns;
    std::vector<xptr> temp_docs;
    inscmap insc_ns;
    ns_map ns_lib;


    static_context();
    ~static_context();
    void _init_context();
    void _release_resources();
    void clear_context();

    xml_ns*        get_ns_pair(const char*  prefix, const char* uri);
    inline xml_ns* get_ns_pair(std::string& prefix, std::string& uri)
    {
    	return get_ns_pair((prefix.size()>0)?prefix.c_str():NULL,uri.c_str());
    }
    xml_ns*        add_to_context(const char* prefix, const char* uri);
    void           remove_from_context(const char* prefix);
    inline void    remove_from_context(xml_ns* ns)
    { 
        remove_from_context(ns->prefix);
    }


    char * get_uri_by_prefix(const char* _prefix, t_item type) const;
    xml_ns* get_xmlns_by_prefix(const char* _prefix, int count = -1);

    void set_base_uri(const char* _base_uri_);
    void set_default_collation_uri(const char* _default_collation_uri_);

    /// resolves uri and returns collation handler
    /// if uri is NULL, returns default collation
    CollationHandler* get_collation(const char *uri);
    CollationHandler* get_default_collation() { return default_collation_handler; }
};


/*******************************************************************************
 * Dynamic context
 ******************************************************************************/
class dynamic_context
{
    /// Dynamic part
public:
    variable_context var_cxt;
    static_context *st_cxt;

    dynamic_context(static_context *_st_cxt_, int _var_cxt_size_) 
        : st_cxt(_st_cxt_), var_cxt(_var_cxt_size_)
    {
    }

    ~dynamic_context() { /* we do not delete st_cxt here because we manage it some other way */ }



    /// Static part
public:
    static global_variable_context glb_var_cxt;
    static function_context funct_cxt;
    static static_context **st_cxts;
    static int st_cxts_num;
    static int st_cxts_pos;
    // FIXME: hack
    static static_context *unmanaged_st_cxt;

	static CollationManager collation_manager;
    /// Output method (set up in API, so it is one for all modules)
    static se_output_method output_method;

    /// date/time/timezone information
    static XMLDateTime current_datetime;
    static XMLDateTime current_date;
    static XMLDateTime current_time;
    static XMLDateTime implicit_timezone;
    static bool datetime_initialized;

    /// string matcher (symbol substitution in result output)
    static StrMatcher stm;


    static void static_set(int _funcs_num_, int _var_decls_num_, int _st_cxts_num_);
    static void static_clear();


    static static_context *create_static_context()
    {
        U_ASSERT(st_cxts_pos < st_cxts_num);

        static_context *st_cxt = new static_context;
        st_cxts[st_cxts_pos++] = st_cxt;
        return st_cxt;
    }

    static dynamic_context *create_unmanaged(int _var_cxt_size_)
    {
        unmanaged_st_cxt = new static_context;
        return new dynamic_context(unmanaged_st_cxt, _var_cxt_size_);
    }

    static void destroy_unmanaged(dynamic_context *cxt)
    {
        delete unmanaged_st_cxt;
        unmanaged_st_cxt = NULL;
        delete cxt;
    }

    static void global_variables_open()
    {
        glb_var_cxt.open();
    }

    static void global_variables_close()
    {
        glb_var_cxt.close();
    }

    static void add_char_mapping(const char* str, const char* rep_str, int pc =-1)
    {
	    stm.add_str(str, rep_str, pc);
    }


    static void set_datetime();

    static static_context *__static_cxt() { return unmanaged_st_cxt ? unmanaged_st_cxt : st_cxts[st_cxts_num - 1]; }
};


#endif
