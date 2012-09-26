/*
 * File:  PPCalculate.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCALCULATE_H
#define _PPCALCULATE_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/boolean_operations.h"
#include "tr/executor/fo/op_map.h"


class CalcOp
{
private:
    virtual void do_accept(PPVisitor &v) = 0; 
public:
    virtual tuple_cell next(dynamic_context *cxt) = 0;
    virtual CalcOp* copy(arr_of_PPOpIn *children) = 0;
    virtual void reopen() = 0;
    
    void accept(PPVisitor &v)   { do_accept(v); }
    
    CalcOp() {}
    virtual ~CalcOp() {}
};


class UnaryOp : public CalcOp
{
protected:
    CalcOp *child;
    un_op_tuple_cell uf;
    xq_unary_op_type type;

public:
    UnaryOp(CalcOp *_child_, 
            un_op_tuple_cell _uf_, 
            xq_unary_op_type _type_) : child(_child_), uf(_uf_), type(_type_) {}
    ~UnaryOp() { delete child; }
    tuple_cell next(dynamic_context *cxt) { return uf(child->next(cxt)); }
    void reopen() { child->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) { return new UnaryOp(child->copy(children), uf, type); }
    inline xq_unary_op_type get_operation_type() { return type; }
    
private:
    virtual void do_accept(PPVisitor &v);
};

class BinaryOp : public CalcOp
{
protected:
    CalcOp *child1, *child2;
    bin_op_tuple_cell_tuple_cell bf;
    xq_binary_op_type type;

public:
    BinaryOp(CalcOp *_child1_, 
             CalcOp *_child2_, 
             bin_op_tuple_cell_tuple_cell _bf_,
             xq_binary_op_type _type_) : child1(_child1_), child2(_child2_), bf(_bf_), type(_type_) {}
    ~BinaryOp() 
	{ 
		delete child1; 
		delete child2; 
	} 
    tuple_cell next(dynamic_context *cxt) 
    { 
        tuple_cell r1 = child1->next(cxt);
        tuple_cell r2 = child2->next(cxt);
        return bf(r1, r2); 
    }

    void reopen() { child1->reopen(); child2->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) 
    { 
        BinaryOp *res = new BinaryOp(child1, child2, bf, type);
        res->child1 = child1->copy(children);
        res->child2 = child2->copy(children);
        return res;
    }
    inline xq_binary_op_type get_operation_type() { return type; }
    

private:
    virtual void do_accept(PPVisitor &v);

};

class BinaryOpCollation : public CalcOp
{
protected:
    CalcOp *child1, *child2;
    bin_op_tuple_cell_tuple_cell_collation bf;
    xq_binary_op_type type;

public:
    BinaryOpCollation(CalcOp *_child1_, 
                      CalcOp *_child2_, 
                      bin_op_tuple_cell_tuple_cell_collation _bf_,
                      xq_binary_op_type _type_) : child1(_child1_), child2(_child2_), bf(_bf_), type(_type_) {}                      
    ~BinaryOpCollation() 
	{ 
		delete child1; 
		delete child2; 
	} 
    tuple_cell next(dynamic_context *cxt) 
    { 
        tuple_cell r1 = child1->next(cxt);
        tuple_cell r2 = child2->next(cxt);
        return bf(r1, r2, cxt->get_static_context()->get_default_collation());
    }

    void reopen() { child1->reopen(); child2->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *children) 
    { 
        BinaryOpCollation *res = new BinaryOpCollation(child1, child2, bf, type); 
        res->child1 = child1->copy(children);
        res->child2 = child2->copy(children);
        return res;
    }
    inline xq_binary_op_type get_operation_type() { return type; }
    
private:
    virtual void do_accept(PPVisitor &v);
};


class BinaryOpAnd : public CalcOp
{
protected:
    CalcOp *child1, *child2;

public:
    BinaryOpAnd(CalcOp *_child1_, CalcOp *_child2_) : child1(_child1_), child2(_child2_) {}
    ~BinaryOpAnd() { delete child1; delete child2; } 
    tuple_cell next(dynamic_context *cxt) 
    { 
        tuple_cell r1 = child1->next(cxt);
        if (r1.is_eos()) return fn_false();

        r1 = effective_boolean_value(r1);
        if (!r1.get_xs_boolean()) return fn_false();

        tuple_cell r2 = child2->next(cxt);
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
    
private:
    virtual void do_accept(PPVisitor &v);
};

class BinaryOpOr : public CalcOp
{
protected:
    CalcOp *child1, *child2;

public:
    BinaryOpOr(CalcOp *_child1_, CalcOp *_child2_) : child1(_child1_), child2(_child2_) {}
    ~BinaryOpOr() { delete child1; delete child2; } 
    tuple_cell next(dynamic_context *cxt) 
    { 
        tuple_cell r1 = child1->next(cxt);
        if (!r1.is_eos())
        {
            r1 = effective_boolean_value(r1);
            if (r1.get_xs_boolean()) return fn_true();
        }

        tuple_cell r2 = child2->next(cxt);
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
    
private:
    virtual void do_accept(PPVisitor &v);
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
    tuple_cell next(dynamic_context *cxt)
    {
        children->at(i).op->next(t);
        if (t.is_eos()) return tuple_cell::eos();

        tuple_cell tc = children->at(i).get(t);

        children->at(i).op->next(t);
        if (t.is_eos()) return atomize(tc);
        else throw USER_EXCEPTION(XPTY0004);
    }

    void reopen() { children->at(i).op->reopen(); }
    CalcOp* copy(arr_of_PPOpIn *_children_) 
    { 
        LeafAtomOp *res = new LeafAtomOp(_children_, i); 
        return res;
    }

private:
    virtual void do_accept(PPVisitor &v);
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
    tuple_cell next(dynamic_context *cxt) 
    {
        if (!eos_reached) children->at(i).op->reopen();
        return effective_boolean_value(children->at(i), t, eos_reached);
    }

    void reopen() { children->at(i).op->reopen(); eos_reached = true; }
    CalcOp* copy(arr_of_PPOpIn *_children_) 
    { 
        LeafEffectBoolOp *res = new LeafEffectBoolOp(_children_, i);
        return res;
    }
    
private:
    virtual void do_accept(PPVisitor &v);
};


class PPCalculate : public PPIterator
{
private:
    arr_of_PPOpIn *ch_arr;
    CalcOp *tree;
    bool first_time;

    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t) ; 
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPCalculate(dynamic_context *_cxt_,
                operation_info _info_,
                arr_of_PPOpIn *_ch_arr_,
                CalcOp *_tree_);
    virtual ~PPCalculate();
};


#endif
