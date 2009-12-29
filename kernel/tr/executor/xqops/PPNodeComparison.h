/*
 * File:  PPNodeComparison.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPNODECOMP_H
#define _PPNODECOMP_H

#include "common/sedna.h"
#include "tr/executor/base/PPUtils.h"

class PPNodeComparison : public PPIterator
{
protected:
    PPOpIn seq1;
	PPOpIn seq2;
    bool first_time;
    int type;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

    PPNodeComparison(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPOpIn _seq1_, 
                     PPOpIn _seq2_, 
                     int _type_);


public:    
    virtual ~PPNodeComparison();

    /* Factories for Node Comparisons */
    static PPNodeComparison* PPEQNodeComparison(dynamic_context *_cxt_,
                                                operation_info _info_,
                                                PPOpIn _seq1_,
                                                PPOpIn _seq2_); 

	static PPNodeComparison* PPLTNodeComparison(dynamic_context *_cxt_, 
                                                operation_info _info_,
                                                PPOpIn _seq1_,
                                                PPOpIn _seq2_); 

	static PPNodeComparison* PPGTNodeComparison(dynamic_context *_cxt_, 
                                                operation_info _info_,
                                                PPOpIn _seq1_,
                                                PPOpIn _seq2_); 

	static PPNodeComparison* PPANNodeComparison(dynamic_context *_cxt_, 
                                                operation_info _info_,
                                                PPOpIn _seq1_,
                                                PPOpIn _seq2_); 

	
};
#endif /* _PPNODECOMP_H */
