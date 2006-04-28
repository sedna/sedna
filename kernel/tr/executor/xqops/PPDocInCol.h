/*
 * File:  PPDocInCol.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDOCINCOL_H
#define _PPDOCINCOL_H

#include "sedna.h"

#include "PPBase.h"
#include "XPathOnSchema.h"



class PPDocInCol : public PPIterator
{
protected:
    // given parameters
    PPOpIn col_name_op, doc_name_op;
    // obtained parameters and local data
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPDocInCol(variable_context *_cxt_, 
               PPOpIn _col_name_op_,
               PPOpIn _doc_name_op_);
    virtual ~PPDocInCol();


    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};

#endif
