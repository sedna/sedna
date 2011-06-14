/*
 * File:  PPOrderBy.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>

#include "common/sedna.h"
#include "common/bit_set.h"

#include "tr/executor/xqops/PPOrderBy.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/fo/string_operations.h"

using namespace std;

#define CHECK_PTR_AND_CLEAR(ptr) if(ptr != NULL) { delete ptr; ptr = NULL; }

#define GET_DESERIALIZED_VALUES(pValue1, pValue2, type, offset) \
    if(temp1 == NULL) CHECKP(v1); \
    get_deserialized_value((pValue1), (char*)addr1+(offset), (type)); \
    if(temp2 == NULL) CHECKP(v2); \
    get_deserialized_value((pValue2), (char*)addr2+(offset), (type));


/// Returns the least common type that has a gt operator.
/// Throws XPTY0004 if least common type doesn't have a gt operator.

static inline xmlscm_type get_least_common_type_with_gt(xmlscm_type t1, xmlscm_type t2)
{
    xmlscm_type t = evaluate_common_type(t1, t2);
    
    if (t == xs_string  || is_derived_from_xs_string(t) || t == xs_anyURI)
        return xs_string;
    if (t == xs_integer || is_derived_from_xs_integer(t))
        return xs_integer;
    
    switch (t)
    {
    case xs_time                  :
    case xs_date                  :
    case xs_dateTime              :
    case xs_yearMonthDuration     :
    case xs_dayTimeDuration       :
    case xs_float                 :
    case xs_double                :
    case xs_decimal               :
    case xs_boolean               :
        return t;
    default                       :
        throw XQUERY_EXCEPTION2(XPTY0004, "Non-comparable types found while sorting.");
    }
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPOrderBy::PPOrderBy(dynamic_context *_cxt_,
                     operation_info _info_,
                     bool _stable_,
                     PPOpIn _child_,
                     arr_of_orb_modifier _modifiers_,
                     int _data_size_) : PPIterator(_cxt_, _info_, "PPOrderBy"),
        stable(_stable_),
        child(_child_),
        modifiers(_modifiers_),
        data_size(_data_size_),
        data_cells(NULL),
        sort_cells(NULL),
        ss(NULL)
{
    if (modifiers.size() != child.ts - data_size)
        throw USER_EXCEPTION2(SE1003, "Number of modifiers must be equal to the expressions number.");
    
    sort_size = child.ts - data_size;
    types.resize(sort_size);
}

PPOrderBy::~PPOrderBy()
{
    delete child.op;
    child.op = NULL;
}

void PPOrderBy::do_open ()
{
    child.op -> open();
    first_time  = true;
    need_reinit = false;
    need_to_sort= false;
    pos = 0;
    
    data_cells  = se_new sequence(data_size);
    sort_cells  = se_new sequence(sort_size);

    udata.sort      = sort_cells;
    udata.pos       = 0;
    udata.header    = &types;
    udata.modifiers = &modifiers;
    udata.size      = sizeof(int64_t);
    udata.stable    = stable;
}

void PPOrderBy::do_reopen()
{
    child.op->reopen();
    first_time  = true;
    need_reinit = true;
}

void PPOrderBy::do_close()
{
    child.op->close();
    delete data_cells;
    data_cells = NULL;
    delete sort_cells;
    sort_cells = NULL;
    udata.sort = NULL;
    if (ss != NULL)
    {
        delete ss;
        delete serializer;
        ss = NULL, serializer = NULL;
    }
}

void PPOrderBy::do_next (tuple &t)
{

    if (first_time)
    {
        if (need_reinit)
        {
            data_cells -> clear();
            sort_cells -> clear();
            if (serializer != NULL)
            {
                delete serializer;
                delete ss;
                ss = NULL, serializer = NULL;
            }
            pos = 0;
            udata.pos   = 0;
            udata.size  = sizeof(int64_t);
            need_reinit = false;
            need_to_sort= false;
        }

        int i;
        tuple data_tuple(data_size);
        tuple sort_tuple(sort_size);
        tuple source(child.ts);

        for (i = 0; i < sort_size; i++) types.at(i).initialized = false;

        while (true)
        {
            child.op -> next(source);
            if (source.is_eos()) break;

            for (i = 0; i < source.cells_number; i++)
            {
                if (i < data_size) data_tuple.cells[i] = source.cells[i];
                else
                {
                    if (source.cells[i].is_eos())
                        sort_tuple.cells[i - data_size].set_eos();
                    else
                    {
                        tuple_cell tc = source.cells[i];
                        
                        if (tc.is_atomic() && tc.get_atomic_type() == se_sequence)
                            throw XQUERY_EXCEPTION2(XPTY0004, "A sequence of more than one item is not allowed in order by specification.");
                        
                        tc = atomize(tc);
                        sort_tuple.cells[i - data_size] = tc.get_atomic_type() == xs_untypedAtomic ?
                                                          cast_primitive_to_xs_string(tc) : tc ;
                        
                        orb_common_type* ct = &types.at(i - data_size);
                        xmlscm_type t = sort_tuple.cells[i - data_size].get_atomic_type();
                        
                        if (ct->initialized)
                            ct->xtype = get_least_common_type_with_gt(ct->xtype, t);
                        else
                        {
                            /// We need to check if 't' has a gt operator!
                            ct->xtype = get_least_common_type_with_gt(t, t);
                            ct->initialized = true;
                        }
                    }
                }
            }
            
            data_cells -> add(data_tuple);
            sort_cells -> add(sort_tuple);
        }
        
        for (i = 0; i < sort_size; i++)
        {
            orb_common_type* ct = &types.at(i);
            if (!ct->initialized) continue;
            ct->size = ORB_SERIALIZED_SIZE(ct->xtype);
            udata.size += ct->size;
            need_to_sort = true;
        }

        if (need_to_sort)
        {
            udata.bit_set_offset = udata.size;         // offset to the begining of the eos map in each serialized tuple
            udata.size += sort_size / 8;               // additional bytes for serialized bit_set which contains eos bitmap
            if (sort_size % 8 != 0) udata.size++;

            if (udata.size > DATA_BLK_SIZE)
                throw XQUERY_EXCEPTION2(SE1003, "Order by clause contains too many specifications.");

            //Creating serializer and sorted sequence

            serializer = new TupleSerializer(udata.bit_set_offset, udata.modifiers, udata.header, udata.stable, udata.sort, &(udata.pos));
            ss = new SortedSequence(serializer);

            for (i = 0; i < sort_cells->size(); i++)
            {
                sort_cells -> get(sort_tuple, i);
                ss -> add(sort_tuple);
                udata.pos ++;
            }

            ss -> sort();
        }
        first_time = false;
    }
    
    if (need_to_sort)
    {
        ss -> next(t);
        if (t.cells[0].get_atomic_type() != xs_integer && !t.is_eos())
            throw USER_EXCEPTION2(SE1003, "Incorrect serialization/deserialization.");
        if (!t.is_eos())
        {
            data_cells -> get(t, t.cells[0].get_xs_integer());
        }
    }
    else
    {
        if (pos < data_cells -> size())
        {
            data_cells -> get(t, pos);
        }
        pos++;
    }

    if (t.is_eos() || pos > data_cells -> size()) {
        t.set_eos();
        first_time  = true;
        need_reinit = true;
        if (ss != NULL)
        {
            delete ss;
            delete serializer;
            ss = NULL, serializer = NULL;
        }
        data_cells -> clear();
        sort_cells -> clear();
    }
}

PPIterator* PPOrderBy::do_copy(dynamic_context *_cxt_)
{
    PPOrderBy *res = se_new PPOrderBy(_cxt_,
                                      info,
                                      stable,
                                      child,
                                      modifiers,
                                      data_size);

    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPOrderBy::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPSTuple
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPSTuple::PPSTuple(dynamic_context *_cxt_,
                   operation_info _info_,
                   const arr_of_PPOpIn &_ch_arr_) : PPIterator(_cxt_, _info_, "PPSTuple"),
        ch_arr(_ch_arr_),
        lt(1)
{
}

PPSTuple::~PPSTuple()
{
    for (i = 0; i < ch_arr.size(); i++)
    {
        delete (ch_arr[i].op);
        ch_arr[i].op = NULL;
    }
}

void PPSTuple::do_open ()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->open();
    i = 0;
}

void PPSTuple::do_reopen()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->reopen();
    i = 0;
}

void PPSTuple::do_close()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->close();
    i = 0;
    
    /// We clear on close due to these pointers can be used higher on tree in PPSLet.
    /// We can not perform clearing within PPSLet cause the situation when PPSLet.next()
    /// was not called is possible.
    while (seq_ptrs.size())
    {
        sequence *st = seq_ptrs.back();
        delete st;
        seq_ptrs.pop_back();
    }
}

void PPSTuple::do_next(tuple &t)
{
    if (!i)
    {
        t.eos = false;
        for (; i < ch_arr.size(); i++)
        {
            ch_arr[i].op->next(lt);
            if (lt.is_eos())
            {
                t.cells[i] = tuple_cell::eos();
            }
            else
            {
                t.cells[i] = ch_arr[i].get(lt);
                ch_arr[i].op->next(lt);
                if (!lt.is_eos())
                {
                    sequence* st = se_new sequence(1);
                    seq_ptrs.push_back(st); /// Save pointer. Memory will be freed in PPStuple.close().
                    tuple prev_lt(1);
                    prev_lt.copy(t.cells[i]);
                    st -> add(prev_lt);
                    while (!lt.is_eos())
                    {
                        st -> add(lt);
                        ch_arr[i].op->next(lt);
                    }
                    t.cells[i] = tuple_cell::atomic_se_sequence(st);
                }
            }
        }
    }
    else
    {
        t.set_eos();
        i = 0;
    }
}

PPIterator* PPSTuple::do_copy(dynamic_context *_cxt_)
{
    PPSTuple *res = se_new PPSTuple(_cxt_, info, ch_arr);

    for (i = 0; i < ch_arr.size(); i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);

    return res;
}

void PPSTuple::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPSLet
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


PPSLet::PPSLet(dynamic_context *_cxt_,
               operation_info _info_,
               arr_of_var_dsc _var_dscs_, 
               PPOpIn _source_child_, 
               PPOpIn _data_child_) : PPVarIterator(_cxt_, _info_, "PPSLet"),
                                      var_dscs(_var_dscs_),
                                      source_child(_source_child_),
                                      source(_source_child_.ts),
                                      data_child(_data_child_)
{
}

PPSLet::~PPSLet()
{
    delete source_child.op;
    source_child.op = NULL;
    delete data_child.op;
    data_child.op = NULL;
}


void PPSLet::do_open ()
{
    source_child.op->open();
    need_reopen = false;
    first_time = true;
    s = NULL;
    for (size_t i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        p.type = pt_lazy_complex;
        p.op = this;
        p.cvc = se_new complex_var_consumption;
        p.tuple_pos = i;
    }

	data_child.op->open();
}

void PPSLet::do_reopen()
{
    source_child.op->reopen();
    data_child.op->reopen();
    first_time = true;
    if(s != NULL) s = NULL; 
    reinit_consumer_table();
}

void PPSLet::do_close()
{
    source_child.op->close();
    data_child.op->close();
    if(s != NULL) s = NULL;
}

void PPSLet::do_next(tuple &t)
{
    if (need_reopen)
    {
        source_child.op->reopen();
        need_reopen = false;
        first_time  = true;
        if(s != NULL) s = NULL;
        reinit_consumer_table();
    }

    data_child.op->next(t);

    if (t.is_eos()) need_reopen = true;
}

PPIterator* PPSLet::do_copy(dynamic_context *_cxt_)
{
    PPSLet *res = se_new PPSLet(_cxt_, info, var_dscs, source_child, data_child);
    res->source_child.op = source_child.op->copy(_cxt_);
    res->data_child.op = data_child.op->copy(_cxt_);
    return res;
}

void PPSLet::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    source_child.op->accept(v);
    data_child.op->accept(v);
    v.pop();
}

var_c_id PPSLet::do_register_consumer(var_dsc dsc)
{
    complex_var_consumption &cvc = *(cxt->get_var_producer(dsc, var_cxt).cvc);
    cvc.push_back(0);
    return cvc.size() - 1;
}

void PPSLet::do_next(tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->get_var_producer(dsc, var_cxt);
    complex_var_consumption &cvc = *(p.cvc);

    if(first_time)
    {
        source_child.op->next(source);
        tuple_cell tc = source_child.get(source);
        if(tc.is_atomic() && tc.get_atomic_type() == se_sequence)
        {
            s = tc.get_sequence_ptr(); 
            size = s->size();
        }
        else
            size = 1;
        first_time = false;
    }
    
    if (cvc[id] < size)
    {
        if(size != 1) s->get(source, cvc[id]);
        t.copy(source.cells[p.tuple_pos]);
        cvc[id]++;
    }
    else
    {
        t.set_eos();
        cvc[id] = 0;
    }
}

void PPSLet::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_var_producer(dsc, var_cxt).cvc->at(id) = 0;
}

void PPSLet::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPSLet::reinit_consumer_table()
{
    for (size_t i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        for (size_t j = 0; j < p.cvc->size(); j++) p.cvc->at(j) = 0;
    }
}
