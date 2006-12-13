/*
 * File:  PPIf.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __PPIF_H
#define __PPIF_H

#include "sedna.h"
#include "PPBase.h"

class PPIf : public PPIterator
{
private:
    PPOpIn if_child, then_child, else_child;
    PPIterator *data_child;
    tuple if_data;

    bool eos_reached;

    PPIf(dynamic_context *_cxt_,
         PPOpIn _if_child_, 
         PPOpIn _then_child_, 
         PPOpIn _else_child_,
         PPIterator* _data_child_);

    void children(PPOpIn& _if_child_,
                  PPOpIn& _then_child_,
                  PPOpIn& _else_child_)
    {
        _if_child_ = if_child;
        _then_child_ = then_child;
        _else_child_ = else_child;
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);

    PPIf(dynamic_context *_cxt_,
         PPOpIn _if_child_, 
         PPOpIn _then_child_, 
         PPOpIn _else_child_);
    virtual ~PPIf();

    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
};


#endif
