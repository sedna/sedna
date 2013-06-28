/*
 * File:  dynamic_context.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef _DYNAMIC_CONTEXT_H
#define _DYNAMIC_CONTEXT_H

#include <vector>
#include <string>
#include <list>
#include <map>
#include <set>
#include <stack>
#include <algorithm>

#include "common/sedna.h"
#include "common/counted_ptr.h"
#include "common/utils.h"

#include "tr/structures/xmlns.h"

#include "tr/executor/base/SequenceType.h"
#include "tr/executor/base/static_context.h"
#include "tr/executor/base/INamespaceMap.h"

/*******************************************************************************
 * List of classes used
 ******************************************************************************/
class PPIterator;
class PPVarIterator;
class sequence;
class dynamic_context;
class StaticallyKnownNamespaces;

/*******************************************************************************
 * Different auxiliary types
 ******************************************************************************/
/// Declaration of variable descriptor and auxilary structures
typedef unsigned var_dsc;    // var descriptor
typedef unsigned var_c_id;   // var consumption id

#define INVALID_VAR_DSC UINT32_MAX

inline std::string var_dsc2string(var_dsc id)
{
    return uint2string(id);
}

// global-var descriptor
typedef std::pair<dynamic_context *, var_dsc> global_var_dsc;

// every element of the array is the info about consumption of specific consumer
typedef std::vector<bool> simple_var_consumption;
typedef std::vector<unsigned>  complex_var_consumption;
typedef std::list<unsigned>    free_entries_list;


/// function descriptor
typedef std::pair<dynamic_context *, unsigned> function_id;

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
    unsigned tuple_pos;
    xqp_tuple *t;

    producer();
    ~producer();
};

// variable context
struct variable_context
{
    size_t size;          // size of context (number of producers in array)
    producer *producers;    // array of producers

    variable_context() : size(0), producers(NULL) {}

    void setProducers(size_t _size_)
    {
        U_ASSERT(producers == NULL && size == 0);

        size = _size_;
        producers = (size > 0) ? new producer[size] : NULL;
    }

    ~variable_context()
    {
        delete[] producers;
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
    void open();
    void close();
};

struct global_variable_context
{
    std::vector<global_producer> producers; // array of producers

    void open()
    {
        for (size_t i = 0; i < producers.size(); i++)
            producers[i].open();
    }

    void close()
    {
        for (size_t i = 0; i < producers.size(); i++)
            producers[i].close();
    }

    ~global_variable_context();
};

/*******************************************************************************
 * Functions context
 ******************************************************************************/
struct function_declaration
{
    // body info
    sequence_type ret_st;
    PPIterator *op;

    // args info
    unsigned num;
    sequence_type *args;

    // number of local vars in the function
    unsigned vars_total;

    // context reference
    dynamic_context *dyn_cxt;

    // debug info
    std::string func_name, func_name_uri;
    var_map_id_name var_map;

    function_declaration() : op(NULL), num(0), args(NULL) {}
};

struct function_context
{
    std::vector<function_declaration> fun_decls;

    ~function_context();
};

/*******************************************************************************
 * Dynamic context
 ******************************************************************************/
class dynamic_context
{
private:
    var_dsc curr_var_dsc;   // current variable context counter
    var_dsc curr_gvar_dsc;  // global variable context counter
    unsigned curr_func_id;  // function context counter

    global_variable_context glb_var_cxt; // global variables
    function_context funct_cxt; // functions

    /*
     * contexts for library modules
     */
    std::vector<dynamic_context *> child_cxts;

    /*
     * variable contexts:
     *     1) each time a body is cloned, new context is created
     *     2) for each global variable new context is created
     *     3) for query body new context is created
     */
    std::vector<variable_context *> var_cxts;
    variable_context *current_var_cxt;

    static_context *st_cxt;  // corresponding static context
    var_map_id_name var_map; // some debug-explain info

    // some additional stuff
    std::vector<xptr> temp_docs;

    // date/time/timezone information
    XMLDateTime current_datetime;
    XMLDateTime current_date;
    XMLDateTime current_time;
    XMLDateTime implicit_timezone;

    bool datetime_initialized;
public:
    dynamic_context(static_context *_st_cxt_);
    ~dynamic_context();

    /*
     * This function creates new variable context for local vars
     * See var_cxts comment on when we need them
     */
    void reset_local_vars();

    // used to properly set producers when qep tree is already built
    // NOTE: it should not be called after producers were open
    inline void set_producers()
    {
        current_var_cxt->setProducers(curr_var_dsc);
        reset_local_vars();
    }

    inline const var_map_id_name &get_var_map() const
    {
        return var_map;
    }

    inline void add_to_var_map(var_dsc vd, const var_name_exp &vn)
    {
        var_map[vd] = vn;
    }

    inline static_context *get_static_context()
    {
        return st_cxt;
    }

    inline static_context *sc()
    {
        return st_cxt;
    }

    inline void add_function(const function_declaration &fd, unsigned id)
    {
        funct_cxt.fun_decls[id] = fd;
    }

    inline void add_global_var(const global_producer &gp, var_dsc id)
    {
        glb_var_cxt.producers[id] = gp;
    }

    inline unsigned get_new_func_id()
    {
        funct_cxt.fun_decls.push_back(function_declaration());
        return curr_func_id++;
    }

    inline var_dsc get_new_var_id()
    {
        return curr_var_dsc++;
    }

    inline unsigned get_local_vars_number() const
    {
        return curr_var_dsc;
    }

    inline unsigned get_global_vars_number() const
    {
        return curr_gvar_dsc;
    }

    inline unsigned get_global_funcs_number() const
    {
        return curr_func_id;
    }

    inline var_dsc get_new_global_var_id()
    {
        glb_var_cxt.producers.push_back(global_producer());
        return curr_gvar_dsc++;
    }

    inline function_declaration &get_func_decl(unsigned id)
    {
        return funct_cxt.fun_decls.at(id);
    }

    inline producer &get_var_producer(var_dsc id, variable_context *var_cxt)
    {
        U_ASSERT(std::find(var_cxts.begin(), var_cxts.end(), var_cxt) != var_cxts.end());

        return var_cxt->producers[id];
    }

    inline variable_context *get_current_var_context()
    {
        return current_var_cxt;
    }

    inline global_producer &get_global_var_producer(var_dsc id)
    {
        return glb_var_cxt.producers[id];
    }

    void inline add_temporary_doc_node(xptr n)
    {
        temp_docs.push_back(n);
    }

    void add_cdata_section_element(xsd::QName qName)
    {
        st_cxt->get_serialization_options()
          ->cdataSectionElements.insert(qName);
    }

    void global_variables_open();
    void global_variables_close();

    void add_char_mapping(const std::string& from, const std::string& to)
    {
        st_cxt->get_serialization_options()
          ->charmap.insert(std::pair<std::string, std::string>(from, to));
    }

    void add_child_context(dynamic_context *cxt)
    {
        child_cxts.push_back(cxt);
    }

    bool is_datetime_inited() const
    {
        return datetime_initialized;
    }

    const XMLDateTime &get_datetime() const
    {
        return current_datetime;
    }

    const XMLDateTime &get_date() const
    {
        return current_date;
    }

    const XMLDateTime &get_time() const
    {
        return current_time;
    }

    const XMLDateTime &get_timezone() const
    {
        return implicit_timezone;
    }

    void set_datetime();
};

#endif /* _DYNAMIC_CONTEXT_H */
