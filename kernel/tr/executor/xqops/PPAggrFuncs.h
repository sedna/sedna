/*
 * File:  PPAggrFuncs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPAGGRFUNCS_H
#define _PPAGGRFUNCS_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/PPUtils.h"

tuple_cell op_numeric_add(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_divide(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_less_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_numeric_greater_than(const tuple_cell &a1, const tuple_cell &a2);


class AggFnEssence
{
public:
    virtual ~AggFnEssence() {}
    virtual void add_item(const tuple_cell &tc) = 0;
    virtual tuple_cell get_result() = 0;
    virtual void reopen() = 0;
    static  tuple_cell result(sequence *s);
};

///////////////////////////// PPFnCountEssence /////////////////////////////////
class PPFnCountEssence : public AggFnEssence
{
private:
    __int64 n;
public:
    PPFnCountEssence() : n(0) {}
    ~PPFnCountEssence() {}
    void add_item(const tuple_cell &tc) { n++; }
    tuple_cell get_result() { return tuple_cell::atomic(n); }
    void reopen() { n = 0; }
    static tuple_cell result(sequence *s) { return tuple_cell::atomic((__int64)(s->size())); }
};

////////////////////////////// PPFnSumEssence //////////////////////////////////
class PPFnSumEssence : public AggFnEssence
{
private:
    bool empty;
    tuple_cell sum;
public:
    PPFnSumEssence() : empty(true) {}
    ~PPFnSumEssence() {}
    void add_item(const tuple_cell &tc) 
    { 
        if (empty) { empty = false; sum = atomize(tc); }
        else sum = op_numeric_add(sum, atomize(tc)); 
    }
    tuple_cell get_result() { return empty ? tuple_cell::atomic((double)0) : sum; }
    void reopen() { empty = true; }
    static tuple_cell result(sequence *s)
    {
        if (s->size() == 0) return tuple_cell::atomic((double)0);
        tuple t(1);
        tuple_cell sum = atomize(s->get_00());
        for (int i = 1; i < s->size(); i++)
        {
            s->get(t, i);
            sum = op_numeric_add(sum, atomize(t.cells[0])); 
        }
        return sum;
    }
};

////////////////////////////// PPFnAvgEssence //////////////////////////////////
class PPFnAvgEssence : public AggFnEssence
{
private:
    __int64 n;
    tuple_cell sum;
public:
    PPFnAvgEssence() : n(0) {}
    ~PPFnAvgEssence() {}
    void add_item(const tuple_cell &tc) 
    { 
        if (n == 0) sum = atomize(tc);
        else sum = op_numeric_add(sum, atomize(tc)); 
        n++;
    }
    tuple_cell get_result() { return n == 0 ? tuple_cell::eos() : op_numeric_divide(sum, n); }
    void reopen() { n = 0; }
    static tuple_cell result(sequence *s)
    {
        if (s->size() == 0) return tuple_cell::eos();
        tuple t(1);
        tuple_cell sum = atomize(s->get_00());
        for (unsigned int i = 1; i < s->size(); i++)
        {
            s->get(t, i);
            sum = op_numeric_add(sum, atomize(t.cells[0])); 
        }
        return op_numeric_divide(sum, (__int64)(s->size()));
    }
};

////////////////////////////// PPFnMaxEssence //////////////////////////////////
class PPFnMaxEssence : public AggFnEssence
{
private:
    bool empty;
    tuple_cell res;
public:
    PPFnMaxEssence() : empty(true) {}
    ~PPFnMaxEssence() {}
    void add_item(const tuple_cell &tc) 
    { 
        if (empty) { empty = false; res = atomize(tc); }
        else  
        {
            tuple_cell tca = atomize(tc);
            tuple_cell cond = op_numeric_greater_than(tca, res);
            if (cond.get_xs_boolean()) res = tca;
        }
    }
    tuple_cell get_result() { return empty ? tuple_cell::eos() : res; }
    void reopen() { empty = true; }
    static tuple_cell result(sequence *s)
    {
        if (s->size() == 0) return tuple_cell::eos();
        tuple t(1);
        tuple_cell res = atomize(s->get_00());
        for (unsigned int i = 1; i < s->size(); i++)
        {
            s->get(t, i);
            tuple_cell tca = atomize(t.cells[0]);
            tuple_cell cond = op_numeric_greater_than(tca, res);
            if (cond.get_xs_boolean()) res = tca;
        }
        return res;
    }
};

////////////////////////////// PPFnMinEssence //////////////////////////////////
class PPFnMinEssence : public AggFnEssence
{
private:
    bool empty;
    tuple_cell res;
public:
    PPFnMinEssence() : empty(true) {}
    ~PPFnMinEssence() {}
    void add_item(const tuple_cell &tc) 
    { 
        if (empty) { empty = false; res = atomize(tc); }
        else  
        {
            tuple_cell tca = atomize(tc);
            tuple_cell cond = op_numeric_less_than(tca, res);
            if (cond.get_xs_boolean()) res = tca;
        }
    }
    tuple_cell get_result() { return empty ? tuple_cell::eos() : res; }
    void reopen() { empty = true; }
    static tuple_cell result(sequence *s)
    {
        if (s->size() == 0) return tuple_cell::eos();
        tuple t(1);
        tuple_cell res = atomize(s->get_00());
        for (unsigned int i = 1; i < s->size(); i++)
        {
            s->get(t, i);
            tuple_cell tca = atomize(t.cells[0]);
            tuple_cell cond = op_numeric_less_than(tca, res);
            if (cond.get_xs_boolean()) res = tca;
        }
        return res;
    }
};



////////////////////////////////////////////////////////////////////////////////
//////////////////////////// PPAggrFuncContainer ///////////////////////////////
////////////////////////////////////////////////////////////////////////////////
template<class Essence>
class PPAggrFuncContainer : public PPIterator
{
private:
    /* Incoming data */
    PPOpIn child;
    Essence ess;
    bool first_time;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPAggrFuncContainer(dynamic_context *_cxt_, 
                        PPOpIn _child_);
    virtual ~PPAggrFuncContainer();
};

template<class Essence>
PPAggrFuncContainer<Essence>::PPAggrFuncContainer(dynamic_context *_cxt_, 
                                                  PPOpIn _child_) : PPIterator(_cxt_), 
                                                                    child(_child_)
{
}

template<class Essence>
PPAggrFuncContainer<Essence>::~PPAggrFuncContainer()
{
    delete child.op;
    child.op = NULL;
}

template<class Essence>
void PPAggrFuncContainer<Essence>::open()
{
    child.op->open();
    first_time = true;
}

template<class Essence>
void PPAggrFuncContainer<Essence>::reopen()
{
    child.op->reopen();
    ess.reopen();
    first_time = true;
}

template<class Essence>
void PPAggrFuncContainer<Essence>::close ()
{
    child.op->close();
}

template<class Essence>
void PPAggrFuncContainer<Essence>::next(tuple &t)
{
    SET_CURRENT_PP(this);
    
    if (first_time)
    {
        first_time = false;

        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) break;
            ess.add_item(child.get(t));
        }

        tuple_cell tc = ess.get_result();
        if (tc.is_eos()) t.set_eos();
        else t.copy(tc);
    }
    else
    {
        t.set_eos();
        ess.reopen();
        first_time = true;
    }

    RESTORE_CURRENT_PP;
}

template<class Essence>
PPIterator* PPAggrFuncContainer<Essence>::copy(dynamic_context *_cxt_)
{
    PPAggrFuncContainer<Essence> *res = se_new PPAggrFuncContainer<Essence>(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

template<class Essence>
bool PPAggrFuncContainer<Essence>::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPAggrFuncContainer<Essence>*)cur)->children(child);

    void *af_r;
    bool af_s = (child.op->res_fun())(child.op, cxt, af_r);

    if (!af_s) // if expression is not strict
    {
        child.op = (PPIterator*)af_r;
        r = se_new PPAggrFuncContainer<Essence>(cxt, child);
        return false;
    }

    r = se_new sequence(Essence::result((sequence*)af_r));
    return true;
}

////////////////////////////////////////////////////////////////////////////////
///////////////////// AGGREGATE FUNCTIONS //////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
typedef PPAggrFuncContainer<PPFnCountEssence> PPFnCount;
//typedef PPAggrFuncContainer<PPFnSumEssence> PPFnSum;
//typedef PPAggrFuncContainer<PPFnAvgEssence> PPFnAvg;
//typedef PPAggrFuncContainer<PPFnMaxEssence> PPFnMax;
//typedef PPAggrFuncContainer<PPFnMinEssence> PPFnMin;





///////////////////////////////////////////////////////////////////////////////
/// PPFnMaxMin
///////////////////////////////////////////////////////////////////////////////
class PPFnMaxMin : public PPIterator
{
private:
    PPOpIn child;
    PPOpIn collation;
    CollationHandler* handler;
    int i; // 0 means fn:max, 1 means fn:min

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator *copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnMaxMin(dynamic_context *_cxt_,
               int _i_,
               PPOpIn _child_);
    PPFnMaxMin(dynamic_context *_cxt_,
               int _i_,
               PPOpIn _child_,
               PPOpIn _collation_);
    virtual ~PPFnMaxMin();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnSumAvg
///////////////////////////////////////////////////////////////////////////////
class PPFnSumAvg : public PPIterator
{
private:
    PPOpIn child;
    PPOpIn zero;
    bool first_time;
    int i; // 0 means fn:sum, 1 means fn:avg

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator *copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnSumAvg(dynamic_context *_cxt_,
               int _i_,
               PPOpIn _child_);
    PPFnSumAvg(dynamic_context *_cxt_,
               int _i_,
               PPOpIn _child_,
               PPOpIn _zero_);
    virtual ~PPFnSumAvg();
};



#endif

