/*
 * File:  PPSequenceOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPSequenceOps.h"
#include "tr/executor/fo/boolean_operations.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/xqops/PPSLStub.h"
#include "tr/executor/fo/comparison_operations.h"
#include "tr/executor/base/PPUtils.h"
#include <math.h>

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnEmpty
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnEmpty::PPFnEmpty(dynamic_context *_cxt_,
                     PPOpIn _child_) : PPIterator(_cxt_),
                                       child(_child_)
{
}

PPFnEmpty::~PPFnEmpty()
{
    delete child.op;
    child.op = NULL;
}

void PPFnEmpty::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPFnEmpty::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPFnEmpty::close ()
{
    child.op->close();
}

void PPFnEmpty::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        if (!eos_reached) child.op->reopen();

        child.op->next(t);

        if (t.is_eos()) { t.copy(fn_true()); eos_reached = true; }
        else { t.copy(fn_false()); eos_reached = false; }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnEmpty::copy(dynamic_context *_cxt_)
{
    PPFnEmpty *res = se_new PPFnEmpty(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnEmpty::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnEmpty*)cur)->children(child);

    void *em_r;
    bool em_s = (child.op->res_fun())(child.op, cxt, em_r);

    if (!em_s) // if expression is not strict
    { // create PPFnEmpty and transmit state
        child.op = (PPIterator*)em_r;
        r = se_new PPFnEmpty(cxt, child);
        return false;
    }

    if (((sequence*)em_r)->size() == 0)
        r = se_new sequence(fn_true());
    else
        r = se_new sequence(fn_false());

    return true;
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnExists
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnExists::PPFnExists(dynamic_context *_cxt_,
                       PPOpIn _child_) : PPIterator(_cxt_),
                                         child(_child_)
{
}

PPFnExists::~PPFnExists()
{
    delete child.op;
    child.op = NULL;
}

void PPFnExists::open  ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPFnExists::reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPFnExists::close ()
{
    child.op->close();
}

void PPFnExists::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        if (!eos_reached) child.op->reopen();

        child.op->next(t);

        if (t.is_eos()) { t.copy(fn_false()); eos_reached = true; }
        else { t.copy(fn_true()); eos_reached = false; }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnExists::copy(dynamic_context *_cxt_)
{
    PPFnExists *res = se_new PPFnExists(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnExists::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnExists*)cur)->children(child);

    void *ex_r;
    bool ex_s = (child.op->res_fun())(child.op, cxt, ex_r);

    if (!ex_s) // if expression is not strict
    { // create PPFnExists and transmit state
        child.op = (PPIterator*)ex_r;
        r = se_new PPFnExists(cxt, child);
        return false;
    }

    if (((sequence*)ex_r)->size() == 0)
        r = se_new sequence(fn_false());
    else
        r = se_new sequence(fn_true());

    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnItemAt
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnItemAt::PPFnItemAt(dynamic_context *_cxt_,
                       PPOpIn _seq_child_,
                       PPOpIn _pos_child_) : PPIterator(_cxt_),
                                             seq_child(_seq_child_),
                                             pos_child(_pos_child_)
{
}

PPFnItemAt::~PPFnItemAt()
{
    delete seq_child.op;
    seq_child.op = NULL;
    delete pos_child.op;
    pos_child.op = NULL;
}

void PPFnItemAt::open  ()
{
    seq_child.op->open();
    pos_child.op->open();
    first_time = true;
}

void PPFnItemAt::reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    first_time = true;
}

void PPFnItemAt::close ()
{
    seq_child.op->close();
    pos_child.op->close();
}

void PPFnItemAt::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");

        __int64 pos = cast(pos_child.get(t), xs_integer).get_xs_integer();

        pos_child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");
        if (pos < 1) throw USER_EXCEPTION(SE1007);

        for (__int64 i = 1; i <= pos; i++)
        {
            seq_child.op->next(t);
            if (t.is_eos())
            {
                if (i == 1)
                {
                    t.set_eos();
                    first_time = true;
                    return;
                }

                throw USER_EXCEPTION(SE1007);
            }
        }
    }
    else
    {
        first_time = true;
        seq_child.op->reopen();
        t.set_eos();
    }
}

PPIterator* PPFnItemAt::copy(dynamic_context *_cxt_)
{
    PPFnItemAt *res = se_new PPFnItemAt(_cxt_, seq_child, pos_child);
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);

    return res;
}

bool PPFnItemAt::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn seq_child, pos_child;
    ((PPFnItemAt*)cur)->children(seq_child, pos_child);

    void *pos_r;
    bool pos_s = (pos_child.op->res_fun())(pos_child.op, cxt, pos_r);

    if (!pos_s) // if expression is not strict
    { // create PPFnItemAt and transmit state
        seq_child.op = seq_child.op->copy(cxt);
        pos_child.op = (PPIterator*)pos_r;
        r = se_new PPFnItemAt(cxt, seq_child, pos_child);
        return false;
    }

    void *seq_r;
    bool seq_s = (seq_child.op->res_fun())(seq_child.op, cxt, seq_r);

    if (!seq_s) // if expression is not strict
    { // create PPFnItemAt and transmit state
        seq_child.op = (PPIterator*)seq_r;
        pos_child.op = se_new PPSLStub(cxt, pos_child.op->copy(cxt), (sequence*)pos_r);
        r = se_new PPFnItemAt(cxt, seq_child, pos_child);
        return false;
    }

    sequence* pos_seq = (sequence*)pos_r;
    if (pos_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");

    __int64 pos = cast(pos_seq->get_00(), xs_integer).get_xs_integer();

    if (pos < 1) throw USER_EXCEPTION(SE1007);

    sequence* seq_seq = (sequence*)seq_r;
    r = se_new sequence(seq_child.ts);
    if (seq_seq->size() == 0) return true;
    if (seq_seq->size() < pos) throw USER_EXCEPTION(SE1007);

    ((sequence*)r)->add((*seq_seq)[pos - 1]);
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDistinctValues
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnDistinctValues::PPFnDistinctValues(dynamic_context *_cxt_,
                                       PPOpIn _child_) : PPIterator(_cxt_),
                                                         child(_child_)
{
}

PPFnDistinctValues::PPFnDistinctValues(dynamic_context *_cxt_,
                                       PPOpIn _child_,
                                       PPOpIn _collation_child_) : PPIterator(_cxt_),
                                                                   child(_child_),
                                                                   collation_child(_collation_child_)
{
}

PPFnDistinctValues::~PPFnDistinctValues()
{
    delete child.op;
    child.op = NULL;
    if (collation_child.op)
    {
        delete collation_child.op;
        collation_child.op = NULL;
    }
}

void PPFnDistinctValues::open  ()
{
    child.op->open();
    if (collation_child.op)
        collation_child.op->open();

    s = se_new sequence(1);
    handler = NULL;
    has_NaN = false;
}

void PPFnDistinctValues::reopen()
{
    child.op->reopen();
    if (collation_child.op)
        collation_child.op->reopen();

    s->clear();
    handler = NULL;
    has_NaN = false;
}

void PPFnDistinctValues::close ()
{
    child.op->close();
    if (collation_child.op)
        collation_child.op->close();

    delete s;
    s = NULL;
    handler = NULL;
}

void PPFnDistinctValues::next(tuple &t)
{
    if (!handler) // the same as 'first_time'
    {
        handler = charset_handler->get_unicode_codepoint_collation();

        if (collation_child.op)
        {
            collation_child.op->next(t);
            if(t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument. Argument contains zero items in fn:distinct-values()");

            tuple_cell col = atomize(collation_child.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:distinct-values() (xs_string/derived/promotable is expected)");

            collation_child.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:distinct-values(). Argument contains more than one item");
            
            col = tuple_cell::make_sure_light_atomic(col);
            handler = cxt->st_cxt->get_collation(col.get_str_mem());
        }
    }


    while (true)
    {
        int pos = 0;
        child.op->next(t);

        if (t.is_eos())
        {
            s->clear();
            handler = NULL;
            has_NaN = false;
            return;
        }

        tuple_cell tc = atomize(child.get(t));
        if ((tc.get_atomic_type() == xs_float  && u_is_nan((double)(tc.get_xs_float()))) || 
            (tc.get_atomic_type() == xs_double && u_is_nan(tc.get_xs_double())))
        {
            if (has_NaN) continue;
            else
            {
                has_NaN = true;
                goto store_item_and_return;
            }
        }

        for (pos = 0; pos < s->size(); ++pos)
        {
            s->get(t, pos);
            try {
                tuple_cell comp_res = op_eq(tc, t.cells[0], handler);
                if (comp_res.get_xs_boolean()) break;
            } catch (SednaUserException &e) {
                // continue cycle
            }
        }

        if (pos != s->size()) continue;

    store_item_and_return:
        t.copy(tc);
        s->add(t);
        return;
    }
}

PPIterator* PPFnDistinctValues::copy(dynamic_context *_cxt_)
{
    PPFnDistinctValues *res = se_new PPFnDistinctValues(_cxt_, child, collation_child);
    res->child.op = child.op->copy(_cxt_);
    if (collation_child.op)
        res->collation_child.op = collation_child.op->copy(_cxt_);

    return res;
}

bool PPFnDistinctValues::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnDistinctValues::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnIndexOf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnIndexOf::PPFnIndexOf(dynamic_context *_cxt_,
                         PPOpIn _seq_child_,
                         PPOpIn _srch_child_) : PPIterator(_cxt_),
                                                seq_child(_seq_child_),
                                                srch_child(_srch_child_)
{
}

PPFnIndexOf::PPFnIndexOf(dynamic_context *_cxt_,
                         PPOpIn _seq_child_,
                         PPOpIn _srch_child_,
                         PPOpIn _collation_child_) : PPIterator(_cxt_),
                                                     seq_child(_seq_child_),
                                                     srch_child(_srch_child_),
                                                     collation_child(_collation_child_)
{
}

PPFnIndexOf::~PPFnIndexOf()
{
    delete seq_child.op;
    seq_child.op = NULL;
    delete srch_child.op;
    srch_child.op = NULL;
    if (collation_child.op)
    {
        delete collation_child.op;
        collation_child.op = NULL;
    }
}

void PPFnIndexOf::open  ()
{
    seq_child.op->open();
    srch_child.op->open();
    if (collation_child.op)
        collation_child.op->open();
    handler = NULL;
    pos = 0;
}

void PPFnIndexOf::reopen()
{
    seq_child.op->reopen();
    srch_child.op->reopen();
    if (collation_child.op)
        collation_child.op->reopen();
    handler = NULL;
    pos = 0; 
}

void PPFnIndexOf::close ()
{
    seq_child.op->close();
    srch_child.op->close();
    if (collation_child.op)
        collation_child.op->close();
    handler = NULL;
}

void PPFnIndexOf::next(tuple &t)
{
    if (!handler) // the same as 'first_time'
    {
        handler = charset_handler->get_unicode_codepoint_collation();

        if (collation_child.op)
        {
            collation_child.op->next(t);
            if(t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the third argument. Argument contains zero items in fn:index-of()");

            tuple_cell col = atomize(collation_child.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:index-of() (xs_string/derived/promotable is expected)");

            collation_child.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the third argument in fn:index-of(). Argument contains more than one item");
            
            col = tuple_cell::make_sure_light_atomic(col);
            handler = cxt->st_cxt->get_collation(col.get_str_mem());
        }

        srch_child.op->next(t);
        if(t.is_eos()) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument. Argument contains zero items in fn:index-of()");

        search_param = atomize(srch_child.get(t));

        srch_child.op->next(t);
        if (!t.is_eos()) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:index-of(). Argument contains more than one item");
    }

    while(true)
    {
        seq_child.op->next(t);
        pos++;
        
        if (t.is_eos())
        {
            handler = NULL;
            pos = 0;
            return;
        }

        tuple_cell tc = atomize(seq_child.get(t));

        try 
        {
            tc = op_eq(tc, search_param, handler);
            if (tc.get_xs_boolean())  break; 
        } 
        catch (SednaUserException &e) { /* continue cycle */ }
    }

    t.copy(tuple_cell::atomic(pos));
}

PPIterator* PPFnIndexOf::copy(dynamic_context *_cxt_)
{
    PPFnIndexOf *res = se_new PPFnIndexOf(_cxt_, seq_child, srch_child, collation_child);
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->srch_child.op = srch_child.op->copy(_cxt_);

    if (collation_child.op)
        res->collation_child.op = collation_child.op->copy(_cxt_);

    return res;
}

bool PPFnIndexOf::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnIndexOf::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnReverse
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnReverse::PPFnReverse(dynamic_context *_cxt_,
                         PPOpIn _child_) : PPIterator(_cxt_),
                                           child(_child_),
								           s(NULL)
{
}

PPFnReverse::~PPFnReverse()
{
    delete (child.op);
    child.op = NULL;
}

void PPFnReverse::open ()
{
    child.op->open();
    s = se_new sequence_tmp(child.ts);
    first_time = true;
}

void PPFnReverse::reopen ()
{
    first_time = true;
    s->clear();
}

void PPFnReverse::close ()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPFnReverse::next (tuple &t)
{
    if(first_time)
    {
    	pos = -1;
    	while(true)
    	{
    		child.op->next(t);
    		if(t.is_eos()) break; 
    		s->add(t);
    		pos++;
    	}
    	first_time = false;
    }
    
    if (pos >= 0) s->get(t, pos--);
    else
    {
     	t.set_eos();
     	s->clear();
     	first_time = true;
    }
}

PPIterator* PPFnReverse::copy(dynamic_context *_cxt_)
{
    PPFnReverse *res = se_new PPFnReverse(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPFnReverse::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnReverse::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnSubsequence
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnSubsequence::PPFnSubsequence(dynamic_context *_cxt_,
                                 PPOpIn _seq_child_,
                                 PPOpIn _start_child_) : PPIterator(_cxt_),
                                                         seq_child(_seq_child_),
                                                         start_child(_start_child_),
                                                         is_length(false)
{
}

PPFnSubsequence::PPFnSubsequence(dynamic_context *_cxt_,
                                 PPOpIn _seq_child_,
                                 PPOpIn _start_child_,
                                 PPOpIn _length_child_) : PPIterator(_cxt_),
                                                          seq_child(_seq_child_),
                                                          start_child(_start_child_),
                                                          length_child(_length_child_),
                                                          is_length(true)
{
}


PPFnSubsequence::~PPFnSubsequence()
{
    delete seq_child.op;
    seq_child.op = NULL;
    delete start_child.op;
    start_child.op = NULL;

    if(is_length)
    {
    	delete length_child.op;
    	length_child.op = NULL;
    }
}

void PPFnSubsequence::open  ()
{
    seq_child.op->open();
    start_child.op->open();
    if(is_length) length_child.op->open();
    first_time = true;
}

void PPFnSubsequence::reopen()
{
    seq_child.op->reopen();
    start_child.op->reopen();
    if(is_length) length_child.op->reopen();
    first_time = true;
}

void PPFnSubsequence::close ()
{
    seq_child.op->close();
    start_child.op->close();
    if(is_length) length_child.op->close();
}

void PPFnSubsequence::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        start_child.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:subsequence.");
        
        tuple_cell tc = atomize(start_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();
        
        if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic)) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:subsequence (xs:double or promotable expected).");
        
        start_pos = floor(cast(tc, xs_double).get_xs_double() + 0.5);  //floor(x+0.5) is equal there to fn:round
        
        start_child.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:subsequence.");

        if(is_length)
        {
            length_child.op->next(t);
            if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Empty third argument is not allowed in fn:subsequence.");
            
            tc = atomize(start_child.get(t));
            xtype = tc.get_atomic_type();

            if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic))  
                throw USER_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:subsequence (xs:double or promotable expected).");

            length = floor(cast(tc, xs_double).get_xs_double() + 0.5); //floor(x+0.5) is equal there to fn:round
        
            length_child.op->next(t);
            if (!t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Invalid cardinality of the third argument in fn:subsequence.");
        }
        
        current_pos = 0;           
    }
    
    if(!is_length || length >= 1)  //if length is given it should be greater or equal than 1 to have non-empty sequence as result
    {
       bool length_check = true;   //allows to break evaluation before all input is passed 

       do {
           seq_child.op->next(t);
           current_pos++;
           bool length_check = is_length ? (current_pos < start_pos + length) : true;
           if(!t.is_eos() && start_pos <= current_pos && length_check) return;
       } while( !t.is_eos() && length_check );
    }
    
    t.set_eos();
    first_time = true; 
}

PPIterator* PPFnSubsequence::copy(dynamic_context *_cxt_)
{
    PPFnSubsequence *res = is_length ? se_new PPFnSubsequence(_cxt_, seq_child, start_child, length_child) :
                                       se_new PPFnSubsequence(_cxt_, seq_child, start_child);
                                
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->start_child.op = start_child.op->copy(_cxt_);
    if(is_length) res->length_child.op = length_child.op->copy(_cxt_);

    return res;
}

bool PPFnSubsequence::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnSubsequence::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnRemove
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnRemove::PPFnRemove(dynamic_context *_cxt_,
                       PPOpIn _seq_child_,
                       PPOpIn _pos_child_) : PPIterator(_cxt_),
                                             seq_child(_seq_child_),
                                             pos_child(_pos_child_)
{
}



PPFnRemove::~PPFnRemove()
{
    delete seq_child.op;
    seq_child.op = NULL;
    delete pos_child.op;
    pos_child.op = NULL;
}

void PPFnRemove::open  ()
{
    seq_child.op->open();
    pos_child.op->open();
    first_time = true;
}

void PPFnRemove::reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    first_time = true;
}

void PPFnRemove::close ()
{
    seq_child.op->close();
    pos_child.op->close();
}

void PPFnRemove::next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:remove.");
        
        tuple_cell tc = atomize(pos_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();

        if(!(xtype == xs_untypedAtomic ||
             xtype == xs_integer ||
             is_derived_from_xs_integer(xtype)))  
            throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:remove (xs:untypedAtomic, xs:integer or derived expected).");

        remove_pos = xtype == xs_untypedAtomic ? cast(tc, xs_integer).get_xs_integer() : tc.get_xs_integer(); 
        
        pos_child.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:remove.");

        current_pos = 0;
    }
    
    seq_child.op->next(t);
    current_pos++;

    if(!t.is_eos() && current_pos == remove_pos) 
    {
    	seq_child.op->next(t);
    	current_pos++;
    }
    
    if(t.is_eos()) first_time = true; 
}

PPIterator* PPFnRemove::copy(dynamic_context *_cxt_)
{
    PPFnRemove *res =  se_new PPFnRemove(_cxt_, seq_child, pos_child);
                                
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);

    return res;
}

bool PPFnRemove::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnRemove::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnInsertBefore
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnInsertBefore::PPFnInsertBefore(dynamic_context *_cxt_,
                                   PPOpIn _seq_child_,
                                   PPOpIn _pos_child_,
                                   PPOpIn _ins_child_) : PPIterator(_cxt_),
                                                         seq_child(_seq_child_),
                                                         pos_child(_pos_child_),
                                                         ins_child(_ins_child_)
{
}



PPFnInsertBefore::~PPFnInsertBefore()
{
    delete seq_child.op;
    seq_child.op = NULL;
    delete pos_child.op;
    pos_child.op = NULL;
    delete ins_child.op;
    ins_child.op = NULL;

}

void PPFnInsertBefore::open  ()
{
    seq_child.op->open();
    pos_child.op->open();
    ins_child.op->open();
    first_time = true;
}

void PPFnInsertBefore::reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    ins_child.op->reopen();
    first_time = true;
}

void PPFnInsertBefore::close ()
{
    seq_child.op->close();
    pos_child.op->close();
    ins_child.op->close();
}

void PPFnInsertBefore::next(tuple &t)
{
    if (first_time)
    {
        first_time  = false;
        inserted    = false;
        eos_reached = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:insert-before.");
        
        tuple_cell tc = atomize(pos_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();
        
        if(!(xtype == xs_untypedAtomic ||
             xtype == xs_integer ||
             is_derived_from_xs_integer(xtype)))  
            throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:insert-before (xs:untypedAtomic, xs:integer or derived expected).");

        insert_pos = xtype == xs_untypedAtomic ? cast(tc, xs_integer).get_xs_integer() : tc.get_xs_integer(); 
        
        pos_child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:insert-before.");

        current_pos = 1;
    }
    
    if(!inserted && (current_pos == insert_pos || insert_pos < 1))
    {
    	ins_child.op->next(t);
    	if(t.is_eos()) inserted = true;
    	else return;
    }
        
    if(!eos_reached)
    {
        seq_child.op->next(t);
        current_pos++;
        if(t.is_eos()) eos_reached = true;
    }

    if(!inserted && eos_reached)
    {
    	ins_child.op->next(t);
    	if(t.is_eos()) inserted = true;
    	else return;
    }

    if(inserted && eos_reached) first_time = true;
}

PPIterator* PPFnInsertBefore::copy(dynamic_context *_cxt_)
{
    PPFnInsertBefore *res =  se_new PPFnInsertBefore(_cxt_, seq_child, pos_child, ins_child);
                                
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);
    res->ins_child.op = ins_child.op->copy(_cxt_);

    return res;
}

bool PPFnInsertBefore::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnInsertBefore::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnZeroOrOne
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnZeroOrOne::PPFnZeroOrOne(dynamic_context *_cxt_,
                             PPOpIn _child_) : PPIterator(_cxt_),
                                               child(_child_)
{
}

PPFnZeroOrOne::~PPFnZeroOrOne()
{
    delete child.op;
    child.op = NULL;
}

void PPFnZeroOrOne::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnZeroOrOne::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnZeroOrOne::close ()
{
    child.op->close();
}

void PPFnZeroOrOne::next  (tuple &t)
{
    if(first_time)
    {
        child.op->next(t);
        first_time = false;

        if (!t.is_eos())
        {
            tuple temp(child.ts);
            child.op->next(temp);
            if(!temp.is_eos()) throw USER_EXCEPTION(FORG0003); //error code description: fn:zero-or-one called with a sequence containing more than one item.
        }
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnZeroOrOne::copy(dynamic_context *_cxt_)
{
    PPFnZeroOrOne *res = se_new PPFnZeroOrOne(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnZeroOrOne::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnZeroOrOne::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnOneOrMore
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnOneOrMore::PPFnOneOrMore(dynamic_context *_cxt_,
                             PPOpIn _child_) : PPIterator(_cxt_),
                                               child(_child_)
{
}

PPFnOneOrMore::~PPFnOneOrMore()
{
    delete child.op;
    child.op = NULL;
}

void PPFnOneOrMore::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnOneOrMore::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnOneOrMore::close ()
{
    child.op->close();
}

void PPFnOneOrMore::next  (tuple &t)
{
    child.op->next(t);
    if (t.is_eos()) 
    {
        if(first_time) throw USER_EXCEPTION(FORG0004); //error code description: fn:one-or-more called with a sequence containing no items.
        first_time = true;
    }
    else first_time = false;
}

PPIterator* PPFnOneOrMore::copy(dynamic_context *_cxt_)
{
    PPFnOneOrMore *res = se_new PPFnOneOrMore(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnOneOrMore::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnOneOrMore::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnExactlyOne
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnExactlyOne::PPFnExactlyOne(dynamic_context *_cxt_,
                               PPOpIn _child_) : PPIterator(_cxt_),
                                                 child(_child_)
{
}

PPFnExactlyOne::~PPFnExactlyOne()
{
    delete child.op;
    child.op = NULL;
}

void PPFnExactlyOne::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnExactlyOne::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnExactlyOne::close ()
{
    child.op->close();
}

void PPFnExactlyOne::next  (tuple &t)
{
    if(first_time)
    {
        first_time = false;
        child.op->next(t);
        if(t.is_eos()) throw USER_EXCEPTION2(FORG0005, "Empty sequence is not allowed in fn:exactly-one.");

        tuple temp(child.ts);
        child.op->next(temp);
        if(!temp.is_eos()) throw USER_EXCEPTION2(FORG0005, "More than one item is not allowed in fn:exactly-one.");
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnExactlyOne::copy(dynamic_context *_cxt_)
{
    PPFnExactlyOne *res = se_new PPFnExactlyOne(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnExactlyOne::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnExactlyOne::result");
}

