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
#include "tr/tr_globals.h"



/*******************************************************************************
 * List of classes used
 ******************************************************************************/
class PPIterator;
class PPVisitor;

/*******************************************************************************
 * Global variables
 ******************************************************************************/

namespace tr_globals 
{
    /* 
     * Buffer for strings that fit in main memory (used for various intermediate 
     * operations with strings instead of allocating dynamic memory by new operator)
     */
    extern char mem_str_buf[MAX_ATOMIC_LEX_REPR_SIZE + 1];
    extern char mem_str_buf2[MAX_ATOMIC_LEX_REPR_SIZE + 1];

    /* 
     * Buffer for e_strs (used for various intermediate operations with e_strs
     * instead of allocating dynamic memory by se_new operator)
     */
    extern char e_string_buf[PAGE_SIZE];

    extern TLS_VAR_DECL 
    PPIterator* __current_physop;

    extern TLS_VAR_DECL
    volatile bool is_timer_fired;
    
    /*
     * Signed integer is better here, since in some situations
     * we can get negative value.
     */
    extern TLS_VAR_DECL
    volatile int current_stack_depth;

    /* FIXME: make this TLS_VAR_DECL when we start to use threads */
    extern op_str_buf tmp_op_str_buf;
}

/*******************************************************************************
 * Some macro to throw smart XQUERY_EXCEPTION
 ******************************************************************************/

/* 
 * These macroses must be called on operation enter and exit correspondingly!
 * xxx       - in next(tuple)
 * xxx_VAR   - in next(tuple, var)
 */

#define INCREASE_STACK_DEPTH     tr_globals::current_stack_depth++;

#define DECREASE_STACK_DEPTH     tr_globals::current_stack_depth--;

#define CHECK_STACK_DEPTH        if(tr_globals::current_stack_depth > tr_globals::max_stack_depth) \
                                     throw USER_EXCEPTION2(SE1001,             \
    "Infinite recursion or too complex query. Consider increasing session_stack_depth configuration parameter in sednaconf.xml.");

#define SET_CURRENT_PP(pp)       __current_physop_backup = tr_globals::__current_physop; \
                                 tr_globals::__current_physop = (pp); 
                                 
#define SET_CURRENT_PP_VAR(pp)   __current_physop_backup_var = tr_globals::__current_physop; \
                                 tr_globals::__current_physop = (pp);

#define RESTORE_CURRENT_PP       tr_globals::__current_physop = __current_physop_backup; \
                                 __current_physop_backup = NULL; \

#define RESTORE_CURRENT_PP_VAR   tr_globals::__current_physop = __current_physop_backup_var; \
                                 __current_physop_backup_var = NULL;

/* Must be called after delete qep_tree in trn! */
#define RESET_CURRENT_PP         tr_globals::__current_physop = NULL; \
                                 tr_globals::current_stack_depth = 0;

/* Check in executor if timer is fired */
#define CHECK_TIMER_FLAG         if (tr_globals::is_timer_fired) \
                                     throw USER_EXCEPTION(SE4620);


/*******************************************************************************
 * Struct encapsulates 'in' argument for PPIterator operation
 ******************************************************************************/
struct PPOpIn
{
    /* Physical operation */
    PPIterator *op;
    /* Size of input tuple */
    int ts;

    PPOpIn(PPIterator *_op_, int _ts_) : op(_op_), ts(_ts_) {}
    PPOpIn() : op(NULL), ts(0) {}
    tuple_cell &get(tuple &t) { return t.cells[0]; }
    tuple_cell &get(tuple &t) const { return t.cells[0]; }
};

/* Array of PPOpIn */
typedef std::vector<PPOpIn>			arr_of_PPOpIn;

/* Array of tuple pointers */
typedef std::vector<tuple*>			arr_of_tuple_pointer;

/* Array of var descriptors */
typedef std::vector<var_dsc>		arr_of_var_dsc;


/*******************************************************************************
 * Struct encapsulates physical operation properties
 ******************************************************************************/

struct operation_info
{
    /* Line in the source query this operation corresponds to */
    int query_line;
    int query_col;
};


/*******************************************************************************
 * Base iterator of a physical plan tree
 ******************************************************************************/
class PPIterator
{
protected:
    dynamic_context*  cxt;
    operation_info    info;

    /* 
     * Backs up  __current_physop pointer when operation is called from 
     * the parent operation by next(tuple& t)
     */
    PPIterator* __current_physop_backup;

private:
    virtual void        do_open    ()         = 0;
    virtual void        do_reopen  ()         = 0;
    virtual void        do_close   ()         = 0;
    virtual void        do_next    (tuple &t) = 0;
    virtual PPIterator* do_copy(dynamic_context *_cxt_) = 0;
    virtual void        do_accept  (PPVisitor &v) = 0;

    virtual const operation_info& do_get_operation_info() const {
        return info;
    }

public:
    /* 
     * Initialize operation at the very begining of the query execution.
     * Usually opan() implementation may contain quite heavy operations 
     * like memory allocations.
     */
    inline void        open    ()                   { do_open();   }
    
    /* 
     * Equivalent of the consecutive calls close()-open() but in most cases
     * reopen() is lighter. Usually it may just clean memory which was 
     * allocated in open(). We need this method to reset operation's state
     * during the query execution. 
     */
    inline void        reopen  ()                   { do_reopen(); }

    /* 
     * Free resources operation used before delete this operation.
     * Usually close() deletes memory allocated in open(). 
     */
    inline void        close   ()                   { do_close();  }
    
    /* Saves next portion of the result of this operation in t */
    inline void        next    (tuple &t) 
    { 
        CHECK_TIMER_FLAG
        SET_CURRENT_PP(this)
        INCREASE_STACK_DEPTH
        CHECK_STACK_DEPTH

        do_next(t);  
        
        DECREASE_STACK_DEPTH
        RESTORE_CURRENT_PP
    }
    
    /* 
     * Returns a copy of the operation. This is how XQuery user defined
     * functions work in Sedna. Every time someone calls declared function
     * a copy of function's subtree created by means of recursive copy() 
     * calls.
     */
    inline PPIterator* copy(dynamic_context *_cxt_) { 
        return do_copy(_cxt_); 
    }

    inline const operation_info& get_operation_info() const {
        return do_get_operation_info();
    }

    /* Traverse query tree with visitor */
    inline void accept(PPVisitor &v) 
    {
        do_accept(v);
    }


    PPIterator(dynamic_context *_cxt_, 
               operation_info _info_) : cxt(_cxt_),
                                        info(_info_),
                                        __current_physop_backup(NULL) {}

    virtual ~PPIterator()  {}
};


/*******************************************************************************
 * Class for data producer
 ******************************************************************************/
class PPVarIterator : public PPIterator
{
protected:
    /* 
     * Backs up __current_physop pointer when operation is called from the descendant 
     * variable consumer operation by next(tuple &t, var_dsc dsc, var_c_id id)
     */
    PPIterator* __current_physop_backup_var;

private:
    /* Register consumer of the variable dsc */
    virtual var_c_id do_register_consumer(var_dsc dsc) = 0;
    virtual void do_next(tuple &t, var_dsc dsc, var_c_id id) = 0;
    virtual void do_reopen(var_dsc dsc, var_c_id id) = 0;
    virtual void do_close(var_dsc dsc, var_c_id id) = 0;

    
public:     
    /* Register consumer of the variable dsc */
    inline var_c_id register_consumer(var_dsc dsc) { 
        return do_register_consumer(dsc); 
    } 
    /* Get next value of the variable by id */    
    inline void next(tuple &t, var_dsc dsc, var_c_id id) 
    { 
        CHECK_TIMER_FLAG;
        SET_CURRENT_PP_VAR(this);
        do_next(t, dsc, id);
        RESTORE_CURRENT_PP_VAR;
    }
    /* Set id to the beginning of the sequence */
    inline void reopen(var_dsc dsc, var_c_id id) {
        do_reopen(dsc, id);
    }
    /* Close and release resources */        
    inline void close(var_dsc dsc, var_c_id id) {
        do_close(dsc, id);
    }

    PPVarIterator(dynamic_context *_cxt_, 
                  operation_info _info_) : PPIterator(_cxt_, _info_),  
                                           __current_physop_backup_var(NULL) {}
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
 * SednaXQueryException
 ******************************************************************************/

class SednaXQueryException : public SednaUserException
{
protected:
    int xquery_line;

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
                                                                            xquery_line(0) {
        if(_current_physop_)
        {
            xquery_line = _current_physop_->get_operation_info().query_line;
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
                                                                            xquery_line(0) {
        if(_current_physop_)
        {
            xquery_line = _current_physop_->get_operation_info().query_line;
        }
        RESET_CURRENT_PP;
    }

protected:
    virtual std::string getMsg2() const
    {
        std::string res;
        res += "SEDNA Message: ERROR ";
        res += std::string(user_error_code_entries[internal_code].code) + "\n";
        res += std::string(user_error_code_entries[internal_code].descr) + "\n";
        
        if (err_msg.length() != 0)
        {
            res += "Details: ";
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

/* On Darwin we need this hack to compile Sedna with gcc 4.0.1 */
#if defined(DARWIN)
inline SednaXQueryException __xquery_exception2(const char *file,
                                                const char *func, 
                                                int line,
                                                int code, 
                                                const char *details)
{
    return  (elog(EL_ERROR, ("(%s) %s Details: %s", 
                             user_error_code_entries[code].code, 
                             user_error_code_entries[code].descr,
                             details)), 
             SednaXQueryException(file, func, line, details, code, 
                                  tr_globals::__current_physop));

}
#endif /* DARWIN */

#endif /* _PPBASE_H */

