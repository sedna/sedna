/*
 * File:  PPBase.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBASE_H
#define _PPBASE_H

#include "sedna.h"

#include <vector>
#include <list>
#include <map>

#include "dynamic_context.h"
#include "sequence.h"


/*******************************************************************************
 * List of classes used
 ******************************************************************************/
class PPIterator;




/*******************************************************************************
 * Class incapsulates 'in' argument for PPIterator operation
 ******************************************************************************/
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


/// Array of PPOpIn
typedef std::vector<PPOpIn>			arr_of_PPOpIn;

/// Array of tuple pointers
typedef std::vector<tuple*>			arr_of_tuple_pointer;

/// Array of var descriptors
typedef std::vector<var_dsc>		arr_of_var_dsc;



/*******************************************************************************
 * Base iterator of a physical plan tree
 ******************************************************************************/
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


/*******************************************************************************
 * Class for data producer
 ******************************************************************************/
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


/*******************************************************************************
 * Base abstract class for any query tree
 ******************************************************************************/
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


/*******************************************************************************
 * Base abstract class for any update
 ******************************************************************************/
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

//extern pp_static_context st_ct;


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

