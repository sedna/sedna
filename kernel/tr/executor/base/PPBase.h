/*
 * File:  PPBase.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBASE_H
#define _PPBASE_H

#include "common/sedna.h"

#include <vector>
#include <list>
#include <map>

#include "tr/executor/base/dynamic_context.h"
#include "tr/executor/base/sequence.h"



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
    
    int __xquery_line;
    PPIterator* __current_physop_backup;

public:
    virtual void open          ()         = 0;
    virtual void reopen        ()         = 0;
    virtual void close         ()         = 0;
    virtual strict_fun res_fun ()         = 0;
    virtual void next          (tuple &t) = 0;

    virtual PPIterator* copy(dynamic_context *_cxt_) = 0;

    PPIterator(dynamic_context *_cxt_) : cxt(_cxt_), __xquery_line(0), __current_physop_backup(NULL) {}
	
	virtual void  set_xquery_line(int _xquery_line_){__xquery_line = _xquery_line_;}
	virtual int   get_xquery_line() const           { return __xquery_line; }
	virtual char* get_error_msg()   const           { return NULL; }

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
 * Thread variable to throw smart XQUERY_EXCEPTION
 ******************************************************************************/

extern
#ifdef _MSC_VER
__declspec(thread)
#else
__thread
#endif
PPIterator* __current_physop;


#define SET_CURRENT_PP(pp) __current_physop_backup = __current_physop; __current_physop = (pp);
#define RESTORE_CURRENT_PP __current_physop = __current_physop_backup; __current_physop_backup = NULL;
#define RESET_CURRENT_PP   __current_physop = NULL;

/*******************************************************************************
 * SednaXQueryException
 ******************************************************************************/

class SednaXQueryException : public SednaUserException
{
protected:
    int   xquery_line;
    char* physop_msg;

public:
    SednaXQueryException(const char* _file_, 
                         const char* _function_,
                         int _line_,
                         int _internal_code_,
                         PPIterator* _current_physop_) : SednaUserException(_file_,
                                                                            _function_,
                                                                            _line_,
                                                                            "",
                                                                            _internal_code_),
                                                                            xquery_line(0),
                                                                            physop_msg(NULL) {
    if(_current_physop_)
    {
        xquery_line = _current_physop_->get_xquery_line();
        physop_msg  = _current_physop_->get_error_msg();
    }
    RESET_CURRENT_PP;
}
    SednaXQueryException(const char* _file_, 
                         const char* _function_,
                         int _line_,
                         const char* _err_msg_,
                         int _internal_code_,
                         PPIterator* _current_physop_) : SednaUserException(_file_,
                                                                            _function_,
                                                                            _line_,
                                                                            _err_msg_,
                                                                            _internal_code_), 
                                                                            xquery_line(0),
                                                                            physop_msg(NULL) {
    if(_current_physop_)
    {
        xquery_line = _current_physop_->get_xquery_line();
        physop_msg  = _current_physop_->get_error_msg();
    }
    RESET_CURRENT_PP;
}


    virtual std::string getMsg() const
    {
        std::string res;
        res += "SEDNA Message: ERROR ";
        res += std::string(user_error_code_entries[internal_code].code) + "\n";
        res += std::string(user_error_code_entries[internal_code].descr) + "\n";
        
        if (err_msg.length() != 0)
        {
            res += "Details: ";
            if(physop_msg != NULL) {res += physop_msg; res += " ";}
            res += err_msg + "\n";
        }
        if (xquery_line != 0)
            res += "Query line: " + int2string(xquery_line) + "\n";
#if (EL_DEBUG == 1)
        res += "Position: [" + file + ":" + function + ":" + int2string(line) + "]\n";
#endif
        return res;
    }
};



/*******************************************************************************
 * Global variables and corresponding structures and functions
 ******************************************************************************/


/// type of the entity that is stored in database
enum db_entity_type { dbe_document,		// document
                      dbe_collection,	// collection
                      dbe_module        // module
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
/// operations with strings instead of allocating dynamic memory by se_new operator)
extern char mem_str_buf[MAX_MEM_STR_SIZE + 1];

extern char mem_str_buf2[MAX_MEM_STR_SIZE + 1];

/// buffer for e_strs (used for various intermediate operations with e_strs 
/// instead of allocating dynamic memory by se_new operator)
extern char e_string_buf[PAGE_SIZE];



}



#endif

