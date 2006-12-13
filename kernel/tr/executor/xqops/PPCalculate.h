/*
 * File:  PPCalculate.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCALCULATE_H
#define _PPCALCULATE_H

#include "sedna.h"

#include "PPBase.h"
#include "PPUtils.h"
#include "boolean_operations.h"



typedef tuple_cell (*unary_func)(const tuple_cell&);
typedef tuple_cell (*binary_func)(const tuple_cell&, const tuple_cell&);



class CalcOp
{
public:
    virtual tuple_cell next() = 0;
    virtual tuple_cell result(/**/std::vector<void*>& v/**/) = 0;
    virtual CalcOp* copy(arr_of_PPOpIn *children) = 0;
    virtual void reopen() = 0;
    CalcOp() {}
    virtual ~CalcOp() {}
};


class UnaryOp : public CalcOp
{
protected:
    CalcOp *child;
    unary_func uf;

public:
    UnaryOp(CalcOp *_child_, unary_func _uf_) : child(_child_), uf(_uf_) {}
    ~UnaryOp() { delete child; }
    tuple_cell next() { return uf(child->next()); }
    tuple_cell result(/**/std::vector<void*>& v/**/) { return uf(child->result(/**/v/**/)); }
    void reopen() { child->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) { return new UnaryOp(child->copy(children), uf); }
};

class BinaryOp : public CalcOp
{
protected:
    CalcOp *child1, *child2;
    binary_func bf;

public:
    BinaryOp(CalcOp *_child1_, 
             CalcOp *_child2_, 
             binary_func _bf_) : child1(_child1_), child2(_child2_), bf(_bf_) {}
    ~BinaryOp() 
	{ 
		delete child1; 
		delete child2; 
	} 
    tuple_cell next() 
    { 
        tuple_cell r1 = child1->next();
        tuple_cell r2 = child2->next();
        return bf(r1, r2); 
    }
    tuple_cell result(/**/std::vector<void*>& v/**/) 
    { 
        tuple_cell r1 = child1->result(/**/v/**/);
        tuple_cell r2 = child2->result(/**/v/**/);
        return bf(r1, r2);
    }
    void reopen() { child1->reopen(); child2->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) 
    { 
        BinaryOp *res = new BinaryOp(child1, child2, bf); 
        res->child1 = child1->copy(children);
        res->child2 = child2->copy(children);
        return res;
    }
};

class BinaryOpAnd : public CalcOp
{
protected:
    CalcOp *child1, *child2;

public:
    BinaryOpAnd(CalcOp *_child1_, CalcOp *_child2_) : child1(_child1_), child2(_child2_) {}
    ~BinaryOpAnd() { delete child1; delete child2; } 
    tuple_cell next() 
    { 
        tuple_cell r1 = child1->next();
        if (r1.is_eos()) return fn_false();

        r1 = effective_boolean_value(r1);
        if (!r1.get_xs_boolean()) return fn_false();

        tuple_cell r2 = child2->next();
        if (r2.is_eos()) return fn_false();

        r2 = effective_boolean_value(r2);
        return r2; 
    }
    tuple_cell result(/**/std::vector<void*>& v/**/) 
    { 
        tuple_cell r1 = child1->result(/**/v/**/);
        if (r1.is_eos()) return fn_false();

        r1 = effective_boolean_value(r1);
        if (!r1.get_xs_boolean()) return fn_false();

        tuple_cell r2 = child2->result(/**/v/**/);
        if (r2.is_eos()) return fn_false();

        r2 = effective_boolean_value(r2);
        return r2; 
    }
    void reopen() { child1->reopen(); child2->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) 
    { 
        BinaryOpAnd *res = new BinaryOpAnd(child1, child2); 
        res->child1 = child1->copy(children);
        res->child2 = child2->copy(children);
        return res;
    }
};

class BinaryOpOr : public CalcOp
{
protected:
    CalcOp *child1, *child2;

public:
    BinaryOpOr(CalcOp *_child1_, CalcOp *_child2_) : child1(_child1_), child2(_child2_) {}
    ~BinaryOpOr() { delete child1; delete child2; } 
    tuple_cell next() 
    { 
        tuple_cell r1 = child1->next();
        if (!r1.is_eos())
        {
            r1 = effective_boolean_value(r1);
            if (r1.get_xs_boolean()) return fn_true();
        }

        tuple_cell r2 = child2->next();
        if (r2.is_eos()) return fn_false();

        r2 = effective_boolean_value(r2);
        return r2; 
    }
    tuple_cell result(/**/std::vector<void*>& v/**/) 
    { 
        tuple_cell r1 = child1->result(/**/v/**/);
        if (!r1.is_eos())
        {
            r1 = effective_boolean_value(r1);
            if (r1.get_xs_boolean()) return fn_true();
        }

        tuple_cell r2 = child2->result(/**/v/**/);
        if (r2.is_eos()) return fn_false();

        r2 = effective_boolean_value(r2);
        return r2; 
    }
    void reopen() { child1->reopen(); child2->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) 
    { 
        BinaryOpOr *res = new BinaryOpOr(child1, child2); 
        res->child1 = child1->copy(children);
        res->child2 = child2->copy(children);
        return res;
    }
};


class LeafAtomOp : public CalcOp
{
protected:
    arr_of_PPOpIn *children;
    int i;
    tuple t;

public:
    LeafAtomOp(arr_of_PPOpIn *_children_,
               int _i_) : children(_children_), 
                          i(_i_),
                          t(_children_->at(i).ts) {}
    ~LeafAtomOp() {}
    tuple_cell next()
    {
        children->at(i).op->next(t);
        if (t.is_eos()) return tuple_cell::eos();

        tuple_cell tc = children->at(i).get(t);

        children->at(i).op->next(t);
        if (t.is_eos()) return atomize(tc);
        else throw USER_EXCEPTION(XPTY0004);
    }
    tuple_cell result(/**/std::vector<void*>& v/**/)
    {
        sequence *s = (sequence*)(v[i]);
        if (s->size() == 0) return tuple_cell::eos();
        if (s->size() != 1) throw USER_EXCEPTION(XPTY0004);
        s->get(t, 0);
        return atomize(t.cells[0]);
    }
    void reopen() { children->at(i).op->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *_children_) 
    { 
        LeafAtomOp *res = new LeafAtomOp(_children_, i); 
        return res;
    }
};

class LeafEffectBoolOp : public CalcOp
{
protected:
    arr_of_PPOpIn *children;
    int i;
    tuple t;
    bool eos_reached;

public:
    LeafEffectBoolOp(arr_of_PPOpIn *_children_,
                     int _i_) : children(_children_), 
                                i(_i_),
                                t(_children_->at(i).ts),
                                eos_reached(true) {}
    ~LeafEffectBoolOp() {}
    tuple_cell next() 
    {
        if (!eos_reached) children->at(i).op->reopen();
        return effective_boolean_value(children->at(i), t, eos_reached);
    }
    tuple_cell result(/**/std::vector<void*>& v/**/) 
    {
        return effective_boolean_value((sequence*)(v[i]));
    }
    void reopen() { children->at(i).op->reopen(); eos_reached = true; }
    CalcOp* copy(arr_of_PPOpIn *_children_) 
    { 
        LeafEffectBoolOp *res = new LeafEffectBoolOp(_children_, i);
        return res;
    }
};


class PPCalculate : public PPIterator
{
private:
    arr_of_PPOpIn *ch_arr;
    CalcOp *tree;
    bool first_time;

    void children(arr_of_PPOpIn *_ch_arr_)
    {
        *_ch_arr_ = *ch_arr;
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPCalculate(dynamic_context *_cxt_,
                arr_of_PPOpIn *_ch_arr_,
                CalcOp *_tree_);
    virtual ~PPCalculate();
};


#endif
