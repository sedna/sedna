/*
 * File:  PPBase.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBASE_H
#define _PPBASE_H

#include <vector>
#include <list>
#include <map>
#include "sedna.h"
#include "sequence.h"
#include "SequenceType.h"
#include "PPStaticContext.h"


/*******************************************************************************
 * List of classes used
 ******************************************************************************/
class PPIterator;
class PPVarIterator;
class PPSLStub;
class PPFunDef;
class PPRoot;

class PPOpIn;

class sequence;
class sequence::iterator;
class dynamic_context;


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

// function descriptor
typedef int function_id;


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
    ~global_producer() { delete op; op = NULL; }
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
 * Dynamic context
 ******************************************************************************/
class dynamic_context_info
{
};

class dynamic_context
{
public:
    variable_context var_cxt;
    dynamic_context_info *info;

    dynamic_context(dynamic_context_info *_info_, int _var_cxt_size_) 
        : info(_info_), var_cxt(_var_cxt_size_)
    {
    }

    dynamic_context(dynamic_context *_cxt_, int _var_cxt_size_) 
        : info(_cxt_->info), var_cxt(_var_cxt_size_)
    { 
    }

    ~dynamic_context() {}




    static global_variable_context glb_var_cxt;
    static function_context funct_cxt;
    static dynamic_context_info **infos;
    static int infos_num;
    static int infos_pos;

    static void static_clear()
    {
        glb_var_cxt.clear();
        funct_cxt.clear();

        for (infos_pos = 0; infos_pos < infos_num; infos_pos++)
        {
            delete infos[infos_pos];
            infos[infos_pos] = NULL;
        }
        infos_num = 0;
        infos_pos = 0;
        delete [] infos;
        infos = NULL;
    }

    static void static_set(int _funcs_num_, int _var_decls_num_, int _infos_num_)
    {
        funct_cxt.set(_funcs_num_);
        glb_var_cxt.set(_var_decls_num_);

        U_ASSERT(_infos_num_ > 0);
        infos_num = _infos_num_;
        infos_pos = 0;
        infos = new dynamic_context_info*[infos_num];
    }

    static dynamic_context_info *create_info()
    {
        U_ASSERT(infos_pos < infos_num);

        dynamic_context_info *info = new dynamic_context_info;
        infos[infos_pos++] = info;
        return info;
    }

    static dynamic_context *create_unmanaged(int _var_cxt_size_)
    {
        return new dynamic_context(new dynamic_context_info, _var_cxt_size_);
    }

    static void destroy_unmanaged(dynamic_context *cxt)
    {
        delete cxt->info;
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
};




/// Array of PPOpIn
typedef std::vector<PPOpIn>			arr_of_PPOpIn;

/// Array of tuple pointers
typedef std::vector<tuple*>			arr_of_tuple_pointer;

/// Array of var descriptors
typedef std::vector<var_dsc>		arr_of_var_dsc;



/// Incapsulates 'in' argument for PPIterator operation
struct PPOpIn
{
    PPIterator *op;	// operation
    int ts;	// size of input tuple

    PPOpIn(PPIterator *_op_, int _ts_) : op(_op_), ts(_ts_) {}
    PPOpIn() : op(NULL), ts(0) {}
    tuple_cell &get(tuple &t) { return t.cells[0]; }
    tuple_cell &get(tuple &t) const { return t.cells[0]; }
};


/// Type for 'result' function
/// return true, if strict
/// r can be sequence* or PPIterator depending on return value
typedef bool (*strict_fun)(PPIterator* cur, dynamic_context *cxt, void*& r);


/// function forms result of the strict operation (e.g. switches operation from
/// strict to lazy if size of the result sequence is too large)
bool strict_op_result(PPIterator* cur, sequence *res_seq, dynamic_context *cxt, void*& r);



/*******************************************************************************
 * Base classes for physical plan operations
 ******************************************************************************/
/// Base iterator of a physical plan tree
class PPIterator
{
protected:
    dynamic_context *cxt;
public:
    virtual void open          ()         = 0;
    virtual void reopen        ()         = 0;
    virtual void close         ()         = 0;
    virtual strict_fun res_fun ()         = 0;
    virtual void next          (tuple &t) = 0;

    virtual PPIterator* copy(dynamic_context *_cxt_) = 0;

    PPIterator(dynamic_context *_cxt_) : cxt(_cxt_) {}
	virtual bool is_const(){return false;}
    virtual ~PPIterator() {}
};

/// class for data producer
class PPVarIterator : public PPIterator
{
public:
    /// register consumer of the variable dsc
    virtual var_c_id register_consumer(var_dsc dsc) = 0;

     /// get next value of the variable by id
    virtual void next(tuple &t, var_dsc dsc, var_c_id id) = 0;

    /// set id to the beginning of the sequence
    virtual void reopen(var_dsc dsc, var_c_id id) = 0;

    /// close and release resources
    virtual void close(var_dsc dsc, var_c_id id) = 0;

    PPVarIterator(dynamic_context *_cxt_) : PPIterator(_cxt_) {}
    virtual ~PPVarIterator() {}
	
};


class PPQueryEssence
{
public:
    virtual void open() = 0;
    virtual void close() = 0;
    virtual void execute() = 0;
    virtual bool supports_next() = 0;
    virtual bool is_update() = 0;

    PPQueryEssence() {}
    virtual ~PPQueryEssence() {}
};

class PPUpdate : public PPQueryEssence
{
public:
    virtual bool supports_next() { return false; }
    virtual bool is_update() { return true; }

    PPUpdate() {}
    virtual ~PPUpdate() {}
};





/*******************************************************************************
 * Global variables and corresponding structures and functions
 ******************************************************************************/


/// type of the entity that is stored in database
enum db_entity_type { dbe_document,		// document
                      dbe_collection	// collection
                    };

/// database entity
struct db_entity
{
    db_entity_type type;		// type of the db entity
    char *name;					// name of the db entity

    ~db_entity() { delete [] name; name = NULL; }
};


namespace tr_globals 
{

extern static_context st_ct;


/// BUFFERS ///////////////////////////////////////////////////////////////////

/// buffer for strings that fit in main memory (used for various intermediate 
/// operations with strings instead of allocating dynamic memory by new operator)
extern char mem_str_buf[MAX_MEM_STR_SIZE + 1];

extern char mem_str_buf2[MAX_MEM_STR_SIZE + 1];

/// buffer for e_strs (used for various intermediate operations with e_strs 
/// instead of allocating dynamic memory by new operator)
extern char e_string_buf[PAGE_SIZE];



}



#endif

