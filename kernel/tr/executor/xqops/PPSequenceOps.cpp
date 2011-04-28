/*
 * File:  PPSequenceOps.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <math.h>

#include "common/sedna.h"

#include "tr/executor/xqops/PPSequenceOps.h"
#include "tr/executor/fo/boolean_operations.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/fo/comparison_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnEmpty
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnEmpty::PPFnEmpty(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnEmpty"),
                                       child(_child_)
{
}

PPFnEmpty::~PPFnEmpty()
{
    delete child.op;
    child.op = NULL;
}

void PPFnEmpty::do_open ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPFnEmpty::do_reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPFnEmpty::do_close()
{
    child.op->close();
}

void PPFnEmpty::do_next (tuple &t)
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

PPIterator* PPFnEmpty::do_copy(dynamic_context *_cxt_)
{
    PPFnEmpty *res = se_new PPFnEmpty(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnEmpty::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnExists
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnExists::PPFnExists(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnExists"),
                                         child(_child_)
{
}

PPFnExists::~PPFnExists()
{
    delete child.op;
    child.op = NULL;
}

void PPFnExists::do_open ()
{
    child.op->open();
    first_time = true;
    eos_reached = true;
}

void PPFnExists::do_reopen()
{
    child.op->reopen();
    first_time = true;
    eos_reached = true;
}

void PPFnExists::do_close()
{
    child.op->close();
}

void PPFnExists::do_next (tuple &t)
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

PPIterator* PPFnExists::do_copy(dynamic_context *_cxt_)
{
    PPFnExists *res = se_new PPFnExists(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnExists::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnItemAt
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnItemAt::PPFnItemAt(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _seq_child_,
                       PPOpIn _pos_child_) : PPIterator(_cxt_, _info_, "PPFnItemAt"),
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

void PPFnItemAt::do_open ()
{
    seq_child.op->open();
    pos_child.op->open();
    first_time = true;
}

void PPFnItemAt::do_reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    first_time = true;
}

void PPFnItemAt::do_close()
{
    seq_child.op->close();
    pos_child.op->close();
}

void PPFnItemAt::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");

        int64_t pos = cast(pos_child.get(t), xs_integer).get_xs_integer();

        pos_child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid argument to fn:item-at");
        if (pos < 1) throw USER_EXCEPTION(SE1007);

        for (int64_t i = 1; i <= pos; i++)
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

PPIterator* PPFnItemAt::do_copy(dynamic_context *_cxt_)
{
    PPFnItemAt *res = se_new PPFnItemAt(_cxt_, info, seq_child, pos_child);
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);
    return res;
}

void PPFnItemAt::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq_child.op->accept(v);
    pos_child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDistinctValues
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

/*Serializer*/
class DV_Serializer: public ITupleSerializer
{
public:
    size_t serialize(const tuple &t, void *buf);
    void deserialize(tuple &t, void *buf, size_t size);
    int compare(void *buf1, size_t size1, void *buf2, size_t size2);
};

size_t DV_Serializer::serialize(const tuple &t, void *buf)
{
    CHECK_TIMER_FLAG;
    tuple_cell tc = t.cells[0];
    memcpy((char *)buf, &tc, sizeof(tuple_cell));
    return sizeof(tuple_cell);
}

void DV_Serializer::deserialize(tuple &t, void* buf, size_t size)
{
    CHECK_TIMER_FLAG;
    tuple_cell tc;
    memcpy(&tc, (char *)buf, sizeof(tuple_cell));
    t.copy(tc);
}

int DV_Serializer::compare(void* buf1, size_t size1, void* buf2, size_t size2)
{
    CHECK_TIMER_FLAG;
    tuple_cell tc1, tc2;
    tuple tmp(1);

    deserialize(tmp, buf1, size1);
    tc1 = tmp.cells[0];

    deserialize(tmp, buf2, size2);
    tc2 = tmp.cells[0];

    try {
        tuple_cell comp_res = op_eq(tc1, tc2, charset_handler->get_unicode_codepoint_collation());
        if (comp_res.get_xs_boolean())
        {
            return 0;
        }
        else
        {
            comp_res = op_gt(tc1, tc2, charset_handler->get_unicode_codepoint_collation());
            return (comp_res.get_xs_boolean()) ? 1 : -1;
        }
    } catch (SednaUserException) {
        return tc1.get_atomic_type() - tc2.get_atomic_type();
    }
}

/*Distinct values*/

PPFnDistinctValues::PPFnDistinctValues(dynamic_context *_cxt_,
                                       operation_info _info_,
                                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnDistinctValues"),
                                                         child(_child_)
{
}

PPFnDistinctValues::PPFnDistinctValues(dynamic_context *_cxt_,
                                       operation_info _info_,
                                       PPOpIn _child_,
                                       PPOpIn _collation_child_) : PPIterator(_cxt_, _info_, "PPFnDistinctValues"),
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

void PPFnDistinctValues::do_open ()
{
    serializer = new DV_Serializer();
    s = new SortedSequence(serializer);
    child.op->open();
    if (collation_child.op)
        collation_child.op->open();

    handler = NULL;
    has_NaN = false;
}

void PPFnDistinctValues::do_reopen()
{
    child.op->reopen();
    if (collation_child.op)
        collation_child.op->reopen();

    s->clear();
    handler = NULL;
    has_NaN = false;
}

void PPFnDistinctValues::do_close()
{
    child.op->close();
    if (collation_child.op)
        collation_child.op->close();

    delete s;
    s = NULL;
    delete serializer;
    serializer = NULL;
    handler = NULL;
}

inline void PPFnDistinctValues::make_heavy_atomic(tuple &t)
{
    if (t.cells[0].get_type() == tc_light_atomic_var_size) {
        xptr txt_ptr = txt_data.append(t.cells[0]);
        tuple_cell tc = tuple_cell::atomic_estr(t.cells[0].get_atomic_type(), t.cells[0].get_strlen(), txt_ptr);
        t.cells[0] = tc;
    }
}

int PPFnDistinctValues::compare_tc(tuple_cell tc1, tuple_cell tc2)
{
    try {
        tuple_cell comp_res = op_eq(tc1, tc2, charset_handler->get_unicode_codepoint_collation());
        if (comp_res.get_xs_boolean())
        {
            return 0;
        }
        else
        {
            comp_res = op_gt(tc1, tc2, charset_handler->get_unicode_codepoint_collation());
            return (comp_res.get_xs_boolean()) ? 1 : -1;
        }
    } catch (SednaUserException) {
        return tc1.get_atomic_type() - tc2.get_atomic_type();
    }
}

void PPFnDistinctValues::do_next(tuple &t)
{
    if (!handler) // the same as 'first_time'
    {
        handler = charset_handler->get_unicode_codepoint_collation();

        if (collation_child.op)
        {
            collation_child.op->next(t);
            if (t.is_eos())
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument. Argument contains zero items in fn:distinct-values()");

            tuple_cell col = atomize(collation_child.get(t));
            if (!is_string_type(col.get_atomic_type()))
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:distinct-values() (xs_string/derived/promotable is expected)");

            collation_child.op->next(t);
            if (!t.is_eos())
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:distinct-values(). Argument contains more than one item");

            col = tuple_cell::make_sure_light_atomic(col);

            int res = cxt->get_static_context()->get_collation(col.get_str_mem(), &handler);
            if (res != 0) throw XQUERY_EXCEPTION2(FOCH0002, (static_context::get_error_description(res) + " in fn:distinct-values().").c_str());

        }

        //Accumulate elements and sort them
        child.op->next(t);
        while (!t.is_eos())
        {
            tuple_cell tc = atomize(child.get(t));
            t.copy(tc);
            if ((tc.get_atomic_type() == xs_float  && u_is_nan((double)(tc.get_xs_float()))) ||
                    (tc.get_atomic_type() == xs_double && u_is_nan(tc.get_xs_double())))
            {
                if (!has_NaN)
                {
                    has_NaN = true;
                    s->add(t);
                }
            }
            else {
                //if type is light_atomic_var_size then converting it to atomic_estr
                make_heavy_atomic(t);
                s->add(t);
            }
            child.op->next(t);
        }
        s->sort();
        first_element = true;
    }

    s->next(t);
    if (first_element)
    {
        ret_val = t.cells[0];
        first_element = false;
        return;
    }
    while (!t.is_eos() && compare_tc(t.cells[0], ret_val) == 0)
    {
        s->next(t);
    }

    if (t.is_eos())
    {
        s->clear();
        handler = NULL;
        has_NaN = false;
    }
    else
    {
        ret_val = t.cells[0];
    }
}

PPIterator* PPFnDistinctValues::do_copy(dynamic_context *_cxt_)
{
    PPFnDistinctValues *res = se_new PPFnDistinctValues(_cxt_, info, child, collation_child);
    res->child.op = child.op->copy(_cxt_);
    if (collation_child.op)
        res->collation_child.op = collation_child.op->copy(_cxt_);
    return res;
}

void PPFnDistinctValues::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    if (collation_child.op)
        collation_child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnIndexOf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnIndexOf::PPFnIndexOf(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _seq_child_,
                         PPOpIn _srch_child_) : PPIterator(_cxt_, _info_, "PPFnIndexOf"),
                                                seq_child(_seq_child_),
                                                srch_child(_srch_child_)
{
}

PPFnIndexOf::PPFnIndexOf(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _seq_child_,
                         PPOpIn _srch_child_,
                         PPOpIn _collation_child_) : PPIterator(_cxt_, _info_, "PPFnIndexOf"),
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

void PPFnIndexOf::do_open ()
{
    seq_child.op->open();
    srch_child.op->open();
    if (collation_child.op)
        collation_child.op->open();
    handler = NULL;
    pos = 0;
}

void PPFnIndexOf::do_reopen()
{
    seq_child.op->reopen();
    srch_child.op->reopen();
    if (collation_child.op)
        collation_child.op->reopen();
    handler = NULL;
    pos = 0; 
}

void PPFnIndexOf::do_close()
{
    seq_child.op->close();
    srch_child.op->close();
    if (collation_child.op)
        collation_child.op->close();
    handler = NULL;
}

void PPFnIndexOf::do_next(tuple &t)
{
    if (!handler) // the same as 'first_time'
    {
        handler = charset_handler->get_unicode_codepoint_collation();

        if (collation_child.op)
        {
            collation_child.op->next(t);
            if(t.is_eos()) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the third argument. Argument contains zero items in fn:index-of()");

            tuple_cell col = atomize(collation_child.get(t));
            if (!is_string_type(col.get_atomic_type())) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:index-of() (xs_string/derived/promotable is expected)");

            collation_child.op->next(t);
            if (!t.is_eos()) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the third argument in fn:index-of(). Argument contains more than one item");
            
            col = tuple_cell::make_sure_light_atomic(col);
            int res = cxt->get_static_context()->get_collation(col.get_str_mem(), &handler);
            if(res != 0) throw XQUERY_EXCEPTION2(FOCH0002, (static_context::get_error_description(res) + " in fn:index-of().").c_str()); 

        }

        srch_child.op->next(t);
        if(t.is_eos()) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument. Argument contains zero items in fn:index-of()");

        search_param = atomize(srch_child.get(t));

        srch_child.op->next(t);
        if (!t.is_eos()) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:index-of(). Argument contains more than one item");
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
        catch (SednaUserException) { /* continue cycle */ }
    }

    t.copy(tuple_cell::atomic(pos));
}

PPIterator* PPFnIndexOf::do_copy(dynamic_context *_cxt_)
{
    PPFnIndexOf *res = se_new PPFnIndexOf(_cxt_, info, seq_child, srch_child, collation_child);
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->srch_child.op = srch_child.op->copy(_cxt_);

    if (collation_child.op)
        res->collation_child.op = collation_child.op->copy(_cxt_);
    return res;
}

void PPFnIndexOf::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq_child.op->accept(v);
    srch_child.op->accept(v);
    if (collation_child.op)
        collation_child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnReverse
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnReverse::PPFnReverse(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnReverse"),
                                           child(_child_),
								           s(NULL)
{
}

PPFnReverse::~PPFnReverse()
{
    delete (child.op);
    child.op = NULL;
}

void PPFnReverse::do_open ()
{
    child.op->open();
    s = se_new sequence(child.ts);
    first_time = true;
}

void PPFnReverse::do_reopen()
{
    first_time = true;
    s->clear();
}

void PPFnReverse::do_close()
{
    child.op->close();

    delete s;
    s = NULL;
}

void PPFnReverse::do_next (tuple &t)
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

PPIterator* PPFnReverse::do_copy(dynamic_context *_cxt_)
{
    PPFnReverse *res = se_new PPFnReverse(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnReverse::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnSubsequence
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnSubsequence::PPFnSubsequence(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _seq_child_,
                                 PPOpIn _start_child_) : PPIterator(_cxt_, _info_, "PPFnSubsequence"),
                                                         seq_child(_seq_child_),
                                                         start_child(_start_child_),
                                                         is_length(false)
{
}

PPFnSubsequence::PPFnSubsequence(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _seq_child_,
                                 PPOpIn _start_child_,
                                 PPOpIn _length_child_) : PPIterator(_cxt_, _info_, "PPFnSubsequence"),
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

void PPFnSubsequence::do_open ()
{
    seq_child.op->open();
    start_child.op->open();
    if(is_length) length_child.op->open();
    first_time = true;
    need_reopen = false;
}

void PPFnSubsequence::do_reopen()
{
    seq_child.op->reopen();
    start_child.op->reopen();
    if(is_length) length_child.op->reopen();
    first_time = true;
    need_reopen = false;
}

void PPFnSubsequence::do_close()
{
    seq_child.op->close();
    start_child.op->close();
    if(is_length) length_child.op->close();
}

void PPFnSubsequence::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        start_child.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:subsequence.");
        
        tuple_cell tc = atomize(start_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();
        
        if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic)) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:subsequence (xs:double or promotable expected).");
        
        start_pos = floor(cast(tc, xs_double).get_xs_double() + 0.5);  //floor(x+0.5) is equal there to fn:round
        
        start_child.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:subsequence.");

        if(is_length)
        {
            length_child.op->next(t);
            if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty third argument is not allowed in fn:subsequence.");
            
            tc = atomize(start_child.get(t));
            xtype = tc.get_atomic_type();

            if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic))  
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:subsequence (xs:double or promotable expected).");

            length = floor(cast(tc, xs_double).get_xs_double() + 0.5); //floor(x+0.5) is equal there to fn:round
        
            length_child.op->next(t);
            if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid cardinality of the third argument in fn:subsequence.");
        }
        
        current_pos = 0;           
    }
    
    if(need_reopen) 
    { 
        seq_child.op->reopen(); 
        need_reopen = false;
    }
    
    if(!is_length || length >= 1)  //if length is given it should be greater or equal than 1 to have non-empty sequence as result
    {
       bool length_check = true;   //allows to break evaluation before all input is passed 

       do {
           seq_child.op->next(t);
           current_pos++;
           length_check = is_length ? (current_pos < start_pos + length) : true;
           if(!t.is_eos() && start_pos <= current_pos && length_check) return;
       } while( !t.is_eos() && length_check );
    }

    if(!t.is_eos()) need_reopen = true;
    t.set_eos();
    first_time = true; 
}

PPIterator* PPFnSubsequence::do_copy(dynamic_context *_cxt_)
{
    PPFnSubsequence *res = is_length ? se_new PPFnSubsequence(_cxt_, info, seq_child, start_child, length_child) :
                                       se_new PPFnSubsequence(_cxt_, info, seq_child, start_child);
                                
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->start_child.op = start_child.op->copy(_cxt_);
    if(is_length) res->length_child.op = length_child.op->copy(_cxt_);
    return res;
}

void PPFnSubsequence::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq_child.op->accept(v);
    start_child.op->accept(v);
    if (is_length)
        length_child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnRemove
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnRemove::PPFnRemove(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _seq_child_,
                       PPOpIn _pos_child_) : PPIterator(_cxt_, _info_, "PPFnRemove"),
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

void PPFnRemove::do_open ()
{
    seq_child.op->open();
    pos_child.op->open();
    first_time = true;
}

void PPFnRemove::do_reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    first_time = true;
}

void PPFnRemove::do_close()
{
    seq_child.op->close();
    pos_child.op->close();
}

void PPFnRemove::do_next(tuple &t)
{
    if (first_time)
    {
        first_time = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:remove.");
        
        tuple_cell tc = atomize(pos_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();

        if(!(xtype == xs_untypedAtomic ||
             xtype == xs_integer ||
             is_derived_from_xs_integer(xtype)))  
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:remove (xs:untypedAtomic, xs:integer or derived expected).");

        remove_pos = xtype == xs_untypedAtomic ? cast(tc, xs_integer).get_xs_integer() : tc.get_xs_integer(); 
        
        pos_child.op->next(t);
        if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:remove.");

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

PPIterator* PPFnRemove::do_copy(dynamic_context *_cxt_)
{
    PPFnRemove *res =  se_new PPFnRemove(_cxt_, info, seq_child, pos_child);
                                
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);
    return res;
}

void PPFnRemove::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq_child.op->accept(v);
    pos_child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnInsertBefore
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnInsertBefore::PPFnInsertBefore(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _seq_child_,
                                   PPOpIn _pos_child_,
                                   PPOpIn _ins_child_) : PPIterator(_cxt_, _info_, "PPFnInsertBefore"),
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

void PPFnInsertBefore::do_open ()
{
    seq_child.op->open();
    pos_child.op->open();
    ins_child.op->open();
    first_time = true;
}

void PPFnInsertBefore::do_reopen()
{
    seq_child.op->reopen();
    pos_child.op->reopen();
    ins_child.op->reopen();
    first_time = true;
}

void PPFnInsertBefore::do_close()
{
    seq_child.op->close();
    pos_child.op->close();
    ins_child.op->close();
}

void PPFnInsertBefore::do_next(tuple &t)
{
    if (first_time)
    {
        first_time  = false;
        inserted    = false;
        eos_reached = false;

        pos_child.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:insert-before.");
        
        tuple_cell tc = atomize(pos_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();
        
        if(!(xtype == xs_untypedAtomic ||
             xtype == xs_integer ||
             is_derived_from_xs_integer(xtype)))  
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:insert-before (xs:untypedAtomic, xs:integer or derived expected).");

        insert_pos = xtype == xs_untypedAtomic ? cast(tc, xs_integer).get_xs_integer() : tc.get_xs_integer(); 
        
        pos_child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:insert-before.");

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

PPIterator* PPFnInsertBefore::do_copy(dynamic_context *_cxt_)
{
    PPFnInsertBefore *res =  se_new PPFnInsertBefore(_cxt_, info, seq_child, pos_child, ins_child);
                                
    res->seq_child.op = seq_child.op->copy(_cxt_);
    res->pos_child.op = pos_child.op->copy(_cxt_);
    res->ins_child.op = ins_child.op->copy(_cxt_);
    return res;
}

void PPFnInsertBefore::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq_child.op->accept(v);
    pos_child.op->accept(v);
    ins_child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnZeroOrOne
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnZeroOrOne::PPFnZeroOrOne(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnZeroOrOne"),
                                               child(_child_)
{
}

PPFnZeroOrOne::~PPFnZeroOrOne()
{
    delete child.op;
    child.op = NULL;
}

void PPFnZeroOrOne::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnZeroOrOne::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnZeroOrOne::do_close()
{
    child.op->close();
}

void PPFnZeroOrOne::do_next (tuple &t)
{
    if(first_time)
    {
        child.op->next(t);
        
        if (!t.is_eos())
        {
            first_time = false;
			tuple temp(child.ts);
            child.op->next(temp);
            /* 
             * Error code description: 
             * fn:zero-or-one called with a sequence containing more than one item.
             */
            if(!temp.is_eos()) throw XQUERY_EXCEPTION(FORG0003);
        }
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnZeroOrOne::do_copy(dynamic_context *_cxt_)
{
    PPFnZeroOrOne *res = se_new PPFnZeroOrOne(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnZeroOrOne::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnOneOrMore
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnOneOrMore::PPFnOneOrMore(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnOneOrMore"),
                                               child(_child_)
{
}

PPFnOneOrMore::~PPFnOneOrMore()
{
    delete child.op;
    child.op = NULL;
}

void PPFnOneOrMore::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnOneOrMore::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnOneOrMore::do_close()
{
    child.op->close();
}

void PPFnOneOrMore::do_next (tuple &t)
{
    child.op->next(t);
    if (t.is_eos()) 
    {
        /* 
         * Error code description:
         * fn:one-or-more called with a sequence containing no items.
         */
        if(first_time) throw XQUERY_EXCEPTION(FORG0004);
        first_time = true;
    }
    else first_time = false;
}

PPIterator* PPFnOneOrMore::do_copy(dynamic_context *_cxt_)
{
    PPFnOneOrMore *res = se_new PPFnOneOrMore(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnOneOrMore::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnExactlyOne
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnExactlyOne::PPFnExactlyOne(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnExactlyOne"),
                                                 child(_child_)
{
}

PPFnExactlyOne::~PPFnExactlyOne()
{
    delete child.op;
    child.op = NULL;
}

void PPFnExactlyOne::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnExactlyOne::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnExactlyOne::do_close()
{
    child.op->close();
}

void PPFnExactlyOne::do_next (tuple &t)
{
    if(first_time)
    {
        first_time = false;
        child.op->next(t);
        if(t.is_eos()) throw XQUERY_EXCEPTION2(FORG0005, "Empty sequence is not allowed in fn:exactly-one.");

        tuple temp(child.ts);
        child.op->next(temp);
        if(!temp.is_eos()) throw XQUERY_EXCEPTION2(FORG0005, "More than one item is not allowed in fn:exactly-one.");
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnExactlyOne::do_copy(dynamic_context *_cxt_)
{
    PPFnExactlyOne *res = se_new PPFnExactlyOne(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnExactlyOne::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}
