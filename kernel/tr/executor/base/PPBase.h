/*
 * File:  PPBase.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBASE_H
#define _PPBASE_H

#include <vector>
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


/*******************************************************************************
 * Different auxiliary types
 ******************************************************************************/
/// Declaration of variable descriptor and auxilary structures
typedef int var_dsc;	// var descriptor
typedef int var_c_id;	// var consumption id
// every element of the array is the info about consumption of specific cosumer
typedef std::vector<bool> simple_var_consumption;

typedef std::vector<int> complex_var_consumption;

struct congen1_usage
{
    int counter;
    int total;
    int over;

    congen1_usage() : counter(1), total(0), over(0) {}
    void reopen() { counter = 1; over = 0; }
};

///Variable context:
// type of a producer
enum producer_type { pt_not_defined,
                     pt_tuple,
                     pt_seq,			// producer is strict operation, so sequence is already built
                     pt_lazy_simple,	// producer is lazy simle
                     pt_lazy_complex,	// producer is lazy complex
                     pt_congen1			// 'global' variable for con-gen1
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
    congen1_usage *c1u;

    producer() : type(pt_not_defined), 
                 s(NULL), 
                 op(NULL), 
                 svc(NULL), 
                 cvc(NULL), 
                 tuple_pos(0), 
                 t(NULL),
                 c1u(NULL) {}
    ~producer()
    {
        switch (type)
        {
            case pt_not_defined	: break;
            case pt_tuple		: delete t; break;
            case pt_seq			: delete s; break;
            case pt_lazy_simple	: delete svc; break;
            case pt_lazy_complex: delete cvc; break;
            case pt_congen1		: delete c1u; break;
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
typedef bool (*strict_fun)(PPIterator* cur, variable_context *cxt, void*& r);


/// function forms result of the strict operation (e.g. switches operation from
/// strict to lazy if size of the result sequence is too large)
bool strict_op_result(PPIterator* cur, sequence *res_seq, variable_context *cxt, void*& r);


/*
// variable consumer descriptor in the form of xxxyyyyyy, 
// where xxx - identifier of variable,
//       yyyyyy - identifier of the consumer of variable
typedef int var_dsc;

// map global variable id to local variable id
typedef std::map<int, int> var_gid2var_lid;

// Consumer Table
typedef std::vector< std::vector<bool> > consumer_table;


// Complex consumer table
typedef std::vector<sequence::iterator> consumers_positions;

struct var_consumption
{
    sequence *seq;
    consumers_positions positions;
};

typedef std::vector<var_consumption> complex_consumer_table;
*/

/// Query prolog:
typedef int function_id;	// function descriptor

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

struct query_prolog
{
    int size;
    function_declaration *fun_decls;

    query_prolog() : size(0), fun_decls(NULL) {}
    query_prolog(int _size_) : size(_size_) { size > 0 ? fun_decls = new function_declaration[size] : fun_decls = NULL; }
    ~query_prolog() { delete [] fun_decls; }
};


/*******************************************************************************
 * Base classes for physical plan operations
 ******************************************************************************/
/// Base iterator of a physical plan tree
class PPIterator
{
protected:
    variable_context *cxt;
public:
    virtual void open          ()         = 0;
    virtual void reopen        ()         = 0;
    virtual void close         ()         = 0;
    virtual strict_fun res_fun ()         = 0;
    virtual void next          (tuple &t) = 0;

    virtual PPIterator* copy(variable_context *_cxt_) = 0;

    PPIterator(variable_context *_cxt_) : cxt(_cxt_) {}
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

    PPVarIterator(variable_context *_cxt_) : PPIterator(_cxt_) {}
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

/// Query prolog (list of functions defined in prolog)
extern query_prolog qp;
extern static_context st_ct;


/// BUFFERS ///////////////////////////////////////////////////////////////////

/// buffer for strings that fit in main memory (used for various intermediate 
/// operations with strings instead of allocating dynamic memory by new operator)
extern char mem_str_buf[MAX_MEM_STR_SIZE + 1];

/// buffer for e_strs (used for various intermediate operations with e_strs 
/// instead of allocating dynamic memory by new operator)
extern char e_string_buf[PAGE_SIZE];



}



#endif

