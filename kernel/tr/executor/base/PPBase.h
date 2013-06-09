/*
 * File:  PPBase.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBASE_H
#define _PPBASE_H

#include <vector>
#include <list>
#include <map>
#include <deque>

#include "common/sedna.h"

#include "tr/executor/base/dynamic_context.h"
#include "tr/executor/base/sequence.h"
#include "tr/tr_globals.h"


#define CRYPTO_MODULE_NAMESPACE "http://sedna.org/crypto"

/*******************************************************************************
 * List of classes used
 ******************************************************************************/
class PPIterator;

/* Because of cyclic dependecies nature of visitor 
 * pattern use name declaration instead of include.
 * Unfortunately, we lose ability to call visitor's
 * methods in PPIterato::accept.  
 */
class PPVisitor;

/*******************************************************************************
 * Global variables
 ******************************************************************************/

namespace executor_globals 
{
    /* 
     * Buffer for strings that fit in main memory (used for various intermediate 
     * operations with strings instead of allocating dynamic memory by new operator)
     */
    extern char mem_str_buf[MAX_ATOMIC_LEX_REPR_SIZE + 1];
    extern char mem_str_buf2[MAX_ATOMIC_LEX_REPR_SIZE + 1];

    extern TLS_VAR_DECL 
    PPIterator* __current_physop;

    /* In this mode each operation collects execution time,
     * i/o statistics, etc to be used in query profiler statement.
     */
    extern TLS_VAR_DECL
    volatile bool profiler_mode;

    extern
    std::vector < xptr > ss_block_pool;

    /*
     * Signed integer is better here, since in some situations
     * we can get negative value.
     */
    extern TLS_VAR_DECL
    volatile int current_stack_depth;

    /* FIXME: make this TLS_VAR_DECL when we start to use threads */
    extern op_str_buf tmp_op_str_buf;

    /* Reinitialize state before next statement execution */
    void on_kernel_statement_end();
}

/*******************************************************************************
 * Some macro to throw smart XQUERY_EXCEPTION
 ******************************************************************************/

/* 
 * These macroses must be called on operation enter and exit correspondingly!
 * xxx       - in next(tuple)
 * xxx_VAR   - in next(tuple, var)
 */

#define INCREASE_STACK_DEPTH     executor_globals::current_stack_depth++;

#define DECREASE_STACK_DEPTH     executor_globals::current_stack_depth--;

#define CHECK_STACK_DEPTH        if(executor_globals::current_stack_depth > tr_globals::max_stack_depth) \
                                     throw USER_EXCEPTION2(SE1001,\
"Infinite recursion or too complex query. Consider increasing\
 session_stack_depth configuration parameter in sednaconf.xml.");

#define SET_CURRENT_PP(pp)       __current_physop_backup = executor_globals::__current_physop; \
                                 executor_globals::__current_physop = (pp); 
                                 
#define SET_CURRENT_PP_VAR(pp)   __current_physop_backup_var = executor_globals::__current_physop; \
                                 executor_globals::__current_physop = (pp);

#define RESTORE_CURRENT_PP       executor_globals::__current_physop = __current_physop_backup; \
                                 __current_physop_backup = NULL; \

#define RESTORE_CURRENT_PP_VAR   executor_globals::__current_physop = __current_physop_backup_var; \
                                 __current_physop_backup_var = NULL;

/* Must be called after delete qep_tree in trn! */
#define RESET_CURRENT_PP         executor_globals::__current_physop = NULL; \
                                 executor_globals::current_stack_depth = 0;


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
typedef std::vector<PPOpIn>                     arr_of_PPOpIn;

/* Array of tuple pointers */
typedef std::vector<tuple*>                     arr_of_tuple_pointer;

/* Array of var descriptors */
typedef std::vector<var_dsc>                    arr_of_var_dsc;


/*******************************************************************************
 * Struct encapsulates physical operation properties
 ******************************************************************************/

struct profile_info
{
    /* Operation execution time in msec */
    u_timeb time;
    /* Operation locks update of the structure.
     * We need this to prevent simultaneous updates in 
     * user defined functions recursive calls. */
    void* lock;
    /* Number of calls */
    int64_t calls;
     
    profile_info(): lock(NULL), 
                    calls(0) 
    { 
        u_timeb_init(&time);  
    }
};

typedef counted_ptr<profile_info> profile_info_ptr;

struct operation_info
{
    /* Line in the source query this operation corresponds to */
    int query_line;
    /* Column in the source query this operation corresponds to */
    int query_col;
    /* Profile information: execution time, read/write blocks, etc*/
    profile_info_ptr profile;
    /* Operation name */
    const char* name;

    operation_info(): query_line(0),
                      query_col(0), 
                      name("PPIterator") {}

    inline void initialize()
    { 
         if(NULL == profile.get()) 
              profile = profile_info_ptr(new profile_info());
    }
};

namespace executor_globals 
{
    /* Physical operations execution stack.
     * It's being filled only when debug mode is turned on by
     * the clien application.*/
    extern std::deque<operation_info> pp_stack;
}
/*******************************************************************************
 * Base iterator of a physical plan tree
 ******************************************************************************/
class PPIterator
{
protected:
    dynamic_context*  cxt;

    /*
     * We need this because of function body copy, which creates new variable context
     * for local func vars and args
     */
    variable_context *var_cxt;

    operation_info    info;
    u_timeb current1, current2;
    
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
    inline void        open    ()                   
    { 
         if(executor_globals::profiler_mode || 1 == tr_globals::debug_mode)
             info.initialize();
         do_open();
    }
    
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
        if(executor_globals::profiler_mode)
        {
            U_ASSERT(info.profile.get() != NULL);
            info.profile->calls++;
            if(NULL == info.profile->lock) 
            {
                info.profile->lock = (void*)this;
                u_ftime(&current1);
            }
        }
        if(1 == tr_globals::debug_mode) 
        {
            U_ASSERT(info.profile.get() != NULL);
            if(!executor_globals::profiler_mode) info.profile->calls++;
            executor_globals::pp_stack.push_back(info);
        }
     
        CHECK_TIMER_FLAG
        SET_CURRENT_PP(this)
        INCREASE_STACK_DEPTH
        CHECK_STACK_DEPTH

        do_next(t);  
        
        if(1 == tr_globals::debug_mode) 
            executor_globals::pp_stack.pop_back();
        DECREASE_STACK_DEPTH
        RESTORE_CURRENT_PP
        if(executor_globals::profiler_mode)
        {
            if((void*)this == info.profile->lock)
            {
                u_ftime(&current2);
                info.profile->time = info.profile->time + (current2 - current1);
                info.profile->lock = NULL;
            }
        }
    }
    
    /* 
     * Returns a copy of the operation. This is how XQuery user defined
     * functions work in Sedna. Every time someone calls declared function
     * a copy of function's subtree is created by means of recursive copy() 
     * calls.
     */
    inline PPIterator* copy(dynamic_context *_cxt_) 
    { 
        if(executor_globals::profiler_mode)
            info.initialize();
        return do_copy(_cxt_); 
    }

    inline const operation_info& get_operation_info() const {
        return do_get_operation_info();
    }

    /* Traverse query tree with a visitor. 
     * Note, that we can't call visitor's operations here.
     */
    inline void accept(PPVisitor &v)                { do_accept (v); }

    PPIterator(dynamic_context *_cxt_, 
               operation_info _info_,
               const char* _name_) : cxt(_cxt_),
                                   info(_info_),
                                    __current_physop_backup(NULL) 
    {
        info.name = _name_;
        var_cxt = cxt->get_current_var_context();
    }

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
                  operation_info _info_,
                  const char* _name_) : PPIterator(_cxt_, _info_, _name_),  
                                         __current_physop_backup_var(NULL) {}
    virtual ~PPVarIterator() {}
};


/*******************************************************************************
 * Base abstract class for any query tree
 ******************************************************************************/
class PPQueryEssence
{
protected:
    operation_info info;
    u_timeb current1, current2;
    
private:
    virtual void do_open()               = 0;
    virtual void do_close()              = 0;
    virtual void do_execute()            = 0;
    virtual void do_accept(PPVisitor &v) = 0;

public:
    inline void open()                   
    { 
        if(executor_globals::profiler_mode || 1 == tr_globals::debug_mode)
             info.initialize();
        do_open();
    }
    
    inline void execute() 
    { 
        if(executor_globals::profiler_mode)
        {
            U_ASSERT(info.profile.get() != NULL);
            info.profile->calls++;
            u_ftime(&current1);
        }
        if(1 == tr_globals::debug_mode) 
        {
            U_ASSERT(info.profile.get() != NULL);
            if(!executor_globals::profiler_mode) info.profile->calls++;
            executor_globals::pp_stack.push_back(info);
        }

        do_execute();
 
        if(1 == tr_globals::debug_mode) 
            executor_globals::pp_stack.pop_back();
        if(executor_globals::profiler_mode)
        {
            u_ftime(&current2);
            info.profile->time = current2 - current1;
        }
    }

    inline void close()                  { do_close();   }
    inline void accept(PPVisitor &v)     { do_accept(v); }

    virtual bool supports_next()         = 0;
    virtual bool is_update()             = 0;

    inline const operation_info& get_operation_info() const {
        return info;
    }

    PPQueryEssence(const char* _name_) 
    { 
        info.name = _name_;
        info.query_col = 0;
        info.query_line = 0;
    }
    virtual ~PPQueryEssence() {}
};


/*******************************************************************************
 * Base abstract class for any update
 ******************************************************************************/
class PPUpdate : public PPQueryEssence
{
public:
    virtual bool supports_next() { return false; }
    virtual bool is_update()     { return true; }

    PPUpdate(const char* _name_): PPQueryEssence(_name_) {}
    virtual ~PPUpdate() {}
};


/*******************************************************************************
 * SednaXQueryException
 ******************************************************************************/

struct xquery_stack_frame_info {

    int xquery_line;
    int xquery_col;
    std::string func_name;

    xquery_stack_frame_info(int line, int col, std::string func_name): xquery_line(line),
                                                                       xquery_col(col),
                                                                       func_name(func_name)
    {}
};

typedef std::deque<xquery_stack_frame_info> xquery_stack_info;

class SednaXQueryException : public SednaUserException
{
private:
    int xquery_line;
    int xquery_col;
    xquery_stack_info stack_info;

    static const size_t MAX_FRAMES_TO_PRINT = 29;
    static const size_t MAX_TOP_FRAMES = 20;
    static const size_t MAX_BOTTOM_FRAMES = MAX_FRAMES_TO_PRINT - MAX_TOP_FRAMES;
    static const size_t MAX_FUNC_NAME_LEN = 50;

    static std::string coordinate2string(int coordinate) {
        return coordinate <= 0 ? std::string("?") : int2string(coordinate);
    }

public:
    void add_frame(const xquery_stack_frame_info& frame)
    {
        stack_info.push_back(frame);
    }

    //Need it because of stack info deque which doesn't have nothrow destr.
    virtual ~SednaXQueryException() throw() {}

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
                                                                            xquery_col(0) {
        if(_current_physop_)
        {
            xquery_line = _current_physop_->get_operation_info().query_line;
            xquery_col  = _current_physop_->get_operation_info().query_col;
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
                                                                            xquery_col(0) {
        if(_current_physop_)
        {
            xquery_line = _current_physop_->get_operation_info().query_line;
            xquery_col  = _current_physop_->get_operation_info().query_col;
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

        res += "Stack trace: \n";
        int previous_line = xquery_line;
        int previous_col = xquery_col;
        if (!stack_info.empty()) {
            xquery_stack_info::const_iterator itr = stack_info.begin();
            size_t counter = 1;
            size_t total_frames = stack_info.size();
            for (; itr != stack_info.end(); ++itr, ++counter) {
                if (total_frames > MAX_FRAMES_TO_PRINT && counter > MAX_TOP_FRAMES && counter <= total_frames - MAX_BOTTOM_FRAMES) {
                    if (counter == MAX_TOP_FRAMES + 1) {
                        res += "\t... " + int2string((int)(total_frames - MAX_FRAMES_TO_PRINT)) + " frame(s) skipped ...\n";
                    }
                } else {
                    res += "\t";
                    std::string func_name = itr->func_name;
                    if (func_name.size() > MAX_FUNC_NAME_LEN) {
                        res += func_name.substr(0, (size_t)(MAX_FUNC_NAME_LEN / 2) - 3) + "..." +
                               func_name.substr(func_name.size() - (size_t)(MAX_FUNC_NAME_LEN / 2));
                    } else {
                        res += func_name;
                    }
                    res += + " at " + coordinate2string(previous_line) + ":" + coordinate2string(previous_col) + "\n";
                }
                previous_line = itr->xquery_line;
                previous_col = itr->xquery_col;
            }
        }
        res += "\t(main module) at " + coordinate2string(previous_line) + ":" + coordinate2string(previous_col) + "\n";

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
                                  executor_globals::__current_physop));

}
#endif /* DARWIN */

#endif /* _PPBASE_H */

