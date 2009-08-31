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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPAggrFuncContainer(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _child_);
    virtual ~PPAggrFuncContainer();
};

template<class Essence>
PPAggrFuncContainer<Essence>::PPAggrFuncContainer(dynamic_context *_cxt_,
                                                  operation_info _info_,
                                                  PPOpIn _child_) : PPIterator(_cxt_, _info_), 
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
void PPAggrFuncContainer<Essence>::do_open()
{
    child.op->open();
    first_time = true;
}

template<class Essence>
void PPAggrFuncContainer<Essence>::do_reopen()
{
    child.op->reopen();
    ess.reopen();
    first_time = true;
}

template<class Essence>
void PPAggrFuncContainer<Essence>::do_close ()
{
    child.op->close();
}

template<class Essence>
void PPAggrFuncContainer<Essence>::do_next(tuple &t)
{
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
}

template<class Essence>
PPIterator* PPAggrFuncContainer<Essence>::do_copy(dynamic_context *_cxt_)
{
    PPAggrFuncContainer<Essence> *res = se_new PPAggrFuncContainer<Essence>(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

////////////////////////////////////////////////////////////////////////////////
///////////////////// AGGREGATE FUNCTIONS //////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
typedef PPAggrFuncContainer<PPFnCountEssence> PPFnCount;



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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnMaxMin(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_);
    PPFnMaxMin(dynamic_context *_cxt_,
               operation_info _info_,
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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnSumAvg(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_);
    PPFnSumAvg(dynamic_context *_cxt_,
               operation_info _info_,
               int _i_,
               PPOpIn _child_,
               PPOpIn _zero_);
    virtual ~PPFnSumAvg();
};



#endif

