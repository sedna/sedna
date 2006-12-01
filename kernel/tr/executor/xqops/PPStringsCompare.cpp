/*
 * File: PPStringsCompare.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPStringsCompare.h"
#include "string_operations.h"
#include "PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnCompare
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnCompare::PPFnCompare(variable_context *_cxt_,
                         PPOpIn _str1_child_,
                         PPOpIn _str2_child_,
                         bool _is_codepoint_equal_) : PPIterator(_cxt_),
                                                      str1_child(_str1_child_),
                                                      str2_child(_str2_child_),
                                                      is_codepoint_equal(_is_codepoint_equal_)
{
}

PPFnCompare::PPFnCompare(variable_context *_cxt_,
                         PPOpIn _str1_child_,
                         PPOpIn _str2_child_,
                         PPOpIn _collation_child_) : PPIterator(_cxt_),
                                                     str1_child(_str1_child_),
                                                     str2_child(_str2_child_),
                                                     collation_child(_collation_child_),
                                                     is_codepoint_equal(false)
{
}

PPFnCompare::~PPFnCompare()
{
    delete str1_child.op;
    str1_child.op = NULL;
    delete str2_child.op;
    str2_child.op = NULL;

    if (collation_child.op)
    {
    	delete collation_child.op;
    	collation_child.op = NULL;
    }
}

void PPFnCompare::open  ()
{
    str1_child.op->open();
    str2_child.op->open();
    if(collation_child.op) collation_child.op->open();
    first_time = true;
}

void PPFnCompare::reopen()
{
    str1_child.op->reopen();
    str2_child.op->reopen();
    if(collation_child.op) collation_child.op->reopen();
    first_time = true;
}

void PPFnCompare::close ()
{
    str1_child.op->close();
    str2_child.op->close();
    if(collation_child.op) collation_child.op->close();
}

void PPFnCompare::next(tuple &t)
{
    if (first_time)
    {
        CollationHandler* handler = is_codepoint_equal ? charset_handler->get_unicode_codepoint_collation() : NULL;

        if (collation_child.op)
        {
            collation_child.op->next(t);
            if(t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the third argument. Argument contains zero items in fn:compare()");

            tuple_cell col = atomize(collation_child.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:compare() (xs_string/derived/promotable is expected)");

            collation_child.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the third argument in fn:compare(). Argument contains more than one item");
            
            col = tuple_cell::make_sure_light_atomic(col);
            handler = tr_globals::st_ct.get_collation(col.get_str_mem());
        }

        tuple_cell tc1, tc2;

        str1_child.op->next(t);
        if (t.is_eos()) 
            return;

        tc1 = atomize(str1_child.get(t));              
        if (!is_string_type(tc1.get_atomic_type())) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid type of the first argument in fn:compare() (xs_string/derived/promotable is expected)");
    
        str1_child.op->next(t);                                                                               
        if (!t.is_eos()) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the first argument in fn:compare(). Argument contains more than one item");

        
        str2_child.op->next(t);
        if (t.is_eos()) 
            return;

        tc2 = atomize(str2_child.get(t));              
        if (!is_string_type(tc2.get_atomic_type())) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:compare() (xs_string/derived/promotable is expected)");
    
        str2_child.op->next(t);                                                                               
        if (!t.is_eos()) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:compare(). Argument contains more than one item");

        first_time = false;
        if (is_codepoint_equal)
            t.copy(tuple_cell::atomic(fn_compare(tc1, tc2, handler) == 0));
        else 
            t.copy(tuple_cell::atomic((__int64)(fn_compare(tc1, tc2, handler))));
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnCompare::copy(variable_context *_cxt_)
{
    PPFnCompare *res = NULL;
    if (collation_child.op)
    {
        res = new PPFnCompare(_cxt_, str1_child, str2_child, is_codepoint_equal);
    }
    else
    {
        res = new PPFnCompare(_cxt_, str1_child, str2_child, collation_child);
    }                                
    res->str1_child.op = str1_child.op->copy(_cxt_);
    res->str2_child.op = str2_child.op->copy(_cxt_);
    if(collation_child.op) res->collation_child.op = collation_child.op->copy(_cxt_);

    return res;
}

bool PPFnCompare::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnCompare::result");
}
