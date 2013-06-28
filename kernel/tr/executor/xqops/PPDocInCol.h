/*
 * File:  PPDocInCol.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDOCINCOL_H
#define _PPDOCINCOL_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"

class PPDocInCol : public PPIterator
{
protected:
    PPOpIn col_name_op;
    PPOpIn doc_name_op;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPDocInCol(dynamic_context *_cxt_, 
               operation_info _info_,
               PPOpIn _col_name_op_,
               PPOpIn _doc_name_op_);
    virtual ~PPDocInCol();
};

#endif /* _PPDOCINCOL_H */
