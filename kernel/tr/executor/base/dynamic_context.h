/*
 * File:  dynamic_context.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DYNAMIC_CONTEXT_H
#define _DYNAMIC_CONTEXT_H

#include <vector>
#include <string>
#include <list>
#include <map>
#include <set>

#include "common/sedna.h"

#include "tr/executor/base/SequenceType.h"
#include "tr/crmutils/str_matcher.h"
#include "tr/strings/utf8.h"
#include "tr/structures/schema.h"



/*******************************************************************************
 * Define error codes for collations resolving.
 * Since F&O and XQuery specs use different error codes
 * we need to transorm these error codes in place
 * (say in PPOrderBy)
 ******************************************************************************/

#define COLLATION_INVALID_URI        ((int) 0x1)
#define COLLATION_MISS               ((int) 0x2)
#define COLLATION_RESOLVE_ERR        ((int) 0x4)


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
typedef int var_dsc;    // var descriptor
typedef int var_c_id;   // var consumption id
// every element of the array is the info about consumption of specific consumer
typedef std::vector<bool> simple_var_consumption;
typedef std::vector<int>  complex_var_consumption;
typedef std::list<int>    free_entries_list;


/// function descriptor
typedef int function_id;


/// namespaces
typedef std::pair<std::string,std::string> str_pair;
typedef std::map< str_pair, xmlns_ptr> ns_map;
typedef std::map<std::string,std::vector<xmlns_ptr> > inscmap;

/// session options
enum se_session_option {se_debug_mode};


/// query prolog enumerations
enum se_output_method  {se_output_method_xml};
enum se_output_indent  {se_output_indent_yes, se_output_indent_no};
enum xq_boundary_space {xq_boundary_space_strip, xq_boundary_space_preserve};
enum xq_ordering_mode  {xq_ordering_mode_ordered, xq_ordering_mode_unordered};
enum xq_empty_order    {xq_empty_order_greatest, xq_empty_order_least};

/// map var_dsc->var_name
typedef std::pair<std::string, std::string> var_name_exp;
typedef std::map<var_dsc, var_name_exp> var_map_id_name;

/*******************************************************************************
 * Variable context
 ******************************************************************************/
// type of a producer
enum producer_type { pt_not_defined,
                     pt_tuple,
                     pt_seq,            // producer is strict operation, so sequence is already built
                     pt_lazy_simple,    // producer is lazy simle
                     pt_lazy_complex,   // producer is lazy complex
                   };

// producer structure
struct producer
{
    producer_type type;             // type of the producer (i.g. lazy or strict)

    sequence *s;                    // pointer to completely built sequence with data
    PPVarIterator *op;              // pointer to operation with next(i) method
    simple_var_consumption *svc;
    complex_var_consumption *cvc;
    int tuple_pos;
    tuple *t;

    producer();
    ~producer();
};

// variable context
struct variable_context
{
    int size;               // size of context (number of producers in array)
    producer *producers;    // array of producers

    variable_context(int _size_) : size(_size_)
    {
        producers = (size > 0) ? new producer[size] : NULL;
    }

    void resetProducers(size_t _size_)
    {
        delete [] producers;
        size = _size_;
        producers = (size > 0) ? new producer[size] : NULL;
    }

    ~variable_context()
    {
        delete [] producers;
    }
};


/*******************************************************************************
 * Global Variable (declare variable) context
 ******************************************************************************/
struct global_producer
{
    PPVarIterator *op;              // pointer to operation with next(i) method
    dynamic_context *cxt;           // for each global producer we have dynamic_context created in por2qep::PPVarDecl
    complex_var_consumption cvc;
    free_entries_list fel;
    std::string var_name, var_name_uri; // name and URI of the variable for explain feature

    global_producer() : op(NULL), cxt(NULL) {}
    ~global_producer();
    void open();
    void close();
};

struct global_variable_context
{
    int size;                   // size of context (number of producers in array)
    global_producer *producers; // array of producers

    global_variable_context() : size(0), producers(NULL) {}
    ~global_variable_context() { clear(); }
    void set(int _size_)
    {
        size = _size_;
        producers = size > 0 ? se_new global_producer[size] : NULL;
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
    std::string func_name, func_name_uri;
    var_map_id_name var_map;

    function_declaration() : num(0), args(NULL), op(NULL) {}
    ~function_declaration();
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
        fun_decls = size > 0 ? se_new function_declaration[size] : NULL;
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
    enum static_context_fields_flags
    {
        SC_BOUNDARY_SPACE         = 0x1,
        SC_DEFAULT_COLLATION_URI  = 0x2,
        SC_BASE_URI               = 0x4,
        SC_CONSTRUCTION           = 0x8,
        SC_ORDERING_MODE          = 0x10,
        SC_EMPTY_ORDER            = 0x20,
        SC_NAMESPACE_PRESERVE     = 0x40,
        SC_NAMESPACE_INHERIT      = 0x80,
        SC_NAMESPACE              = 0x100,
        SC_DEFAULT_NAMESPACE      = 0x200,
        SC_OUTPUT_INDENT          = 0x400
    };

private:
    /// Prolog Declarations
    uint32_t prolog_set_fields;
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


    std::vector<xmlns_ptr>   def_ns;
    std::set<xmlns_ptr>      predefined_ns;
    std::vector<xptr>        temp_docs;
    inscmap                  insc_ns;
    ns_map                   ns_lib;
    inline void set_field_flag(static_context_fields_flags flag) { prolog_set_fields |= flag; }

public:
    static_context();
    ~static_context();

    void _init_context();
    void _release_resources();
    void clear_context();

    void inline add_temporary_doc_node(xptr n) { temp_docs.push_back(n); }

    bool is_field_set_in_prolog(static_context_fields_flags flag) { return (prolog_set_fields & flag) != 0; }

    xmlns_ptr        get_ns_pair(const char*  prefix, const char* uri);
    inline xmlns_ptr get_ns_pair(std::string& prefix, std::string& uri)
    {
        return get_ns_pair((prefix.size()>0)?prefix.c_str():NULL,uri.c_str());
    }
    xmlns_ptr      add_to_context(const char* prefix, const char* uri);
    void           remove_from_context(const char* prefix);
    inline void    remove_from_context(xmlns_ptr ns)
    {
        remove_from_context(ns->prefix);
    }

    xmlns_ptr get_xmlns_by_prefix(const char* _prefix, int count = -1);

    static inline std::string get_error_description(int err_code)
    {
        switch(err_code)
        {
            case COLLATION_INVALID_URI : return std::string("Invalid lexical representation of the give collation URI");
            case COLLATION_MISS        : return std::string("Collation is not statically known");
            case COLLATION_RESOLVE_ERR : return std::string("Collation URI could not be properly resolved");
            default: throw USER_EXCEPTION2(SE1003, "Impossible case in dynamic_context::get_error_description()");
        }
    }

    /// 1. Resolves uri and returns collation handler through "handler".
    /// 2. If "uri" is NULL, returns default collation handler.
    /// 3. Returns 0 if success, else return one of the collation error
    /// codes defined above.
    int get_collation(const char *uri, /* out */ CollationHandler** handler);
    CollationHandler* get_default_collation() { return default_collation_handler; }

    ///Getters and setters for different prolog options
    inline xq_boundary_space get_boundary_space()  { return boundary_space; }
    inline void set_boundary_space(xq_boundary_space bs)  { boundary_space = bs; set_field_flag(SC_BOUNDARY_SPACE); }

    inline const char* get_default_collation_uri() { return default_collation_uri; }
    void set_default_collation_uri(const char* _default_collation_uri_);

    inline const char* get_base_uri() { return base_uri; }
    void set_base_uri(const char* _base_uri_);

    inline bool get_construction_mode() { return preserve_type; }
    inline void set_construction_mode(bool cm) { preserve_type = cm; set_field_flag(SC_CONSTRUCTION); }

    inline xq_ordering_mode get_ordering_mode() { return ordering_mode; }
    inline void set_ordering_mode(xq_ordering_mode om) { ordering_mode = om; set_field_flag(SC_ORDERING_MODE); }

    inline xq_empty_order get_empty_order() { return empty_order; }
    inline void set_empty_order(xq_empty_order eo) { empty_order = eo; set_field_flag(SC_EMPTY_ORDER); }

    inline bool is_namespace_preserve() { return cn_preserve; }
    inline void set_namespace_preserve(bool np) { cn_preserve = np; set_field_flag(SC_NAMESPACE_PRESERVE); }

    inline bool is_namespace_inherit() { return cn_inherit; }
    inline void set_namespace_inherit(bool ni) { cn_inherit = ni; set_field_flag(SC_NAMESPACE_INHERIT); }

    inline se_output_indent get_output_indent() { return output_indent; }
    inline void set_output_indent(se_output_indent oi) { output_indent = oi; set_field_flag(SC_OUTPUT_INDENT); }

    /* Returns defined namespaces except predefined */
    std::vector<xmlns_ptr> get_explicit_namespaces();
    /* Returns current default namespaces or NULL_XMLNS if there's no one */
    xmlns_ptr get_default_namespace();
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
    var_map_id_name var_map;

    dynamic_context(static_context *_st_cxt_, int _var_cxt_size_)
        : var_cxt(_var_cxt_size_), st_cxt(_st_cxt_)
    {
    }

    // used to properly set producers when qep tree is already built
    // before this dynamic context is created with 0 vars (see lr2por visitor)
    // NOTE: this works IFF we don't register producers in pp-class-constructors
    void set_producers(size_t num)
    {
        var_cxt.resetProducers(num);
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
    static std::vector<static_context *> unmanaged_st_cxts;

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

        static_context *st_cxt = se_new static_context;
        st_cxts[st_cxts_pos++] = st_cxt;
        return st_cxt;
    }

    static static_context *create_unmanaged()
    {
        static_context *res;

        res = se_new static_context;

        unmanaged_st_cxts.push_back(res);

        return res;
    }

    static void destroy_unmanaged(dynamic_context *cxt)
    {
//        delete unmanaged_st_cxt;
//        unmanaged_st_cxt = NULL;
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

    static void add_char_mapping(const char* str, const char* rep_str, int pc = -1)
    {
        stm.add_str(str, rep_str, pc);
    }

    static void set_datetime();

    /// Session parameters (they are temporally placed in the dynamic context)
    /// print call stack in case of error
    static int stack_trace_debug;

    static void set_session_option(se_session_option type, const void* s, int n);
    static void reset_session_options();
};


#endif
