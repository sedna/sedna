/*
 * File:  PPSeqChecker.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPSEQCHECKER_H
#define __PPSEQCHECKER_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPSeqChecker : public PPIterator
{
public:
    enum CheckMode
    {
        CHECK_NODE, // check for sequence to contain only nodes (XPTY0019)
        CHECK_MIX, // check for sequence not to contain mix of atomic and nodes (XPTY0018)
    };
    
    static inline const char* 
    CheckMode2string(CheckMode cm) 
    {
        switch(cm)
        {
        case CHECK_NODE: return "node";
        case CHECK_MIX: return "mix";
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in check mode to string conversion");
        }
    }

private:
    PPOpIn child;

    CheckMode mode;
    bool expect_nodes; // in mixed mode expect nodes only
    unsigned int pos;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPSeqChecker(dynamic_context *_cxt_,
            operation_info _info_,
            PPOpIn _child_,
            CheckMode _mode_);

    virtual ~PPSeqChecker();
    inline CheckMode get_check_mode() { return mode; }
};

#endif /* __PPSEQCHECKER_H */
