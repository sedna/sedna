/*
 * File:  PPDebug.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPDebug.h"


PPDebug::PPDebug(dynamic_context *_cxt_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        dostr(dynamic_context::dostr()),
                                                        child_name(_child_name_)
{
}

PPDebug::PPDebug(dynamic_context *_cxt_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_,
                 const str_counted_ptr &_child_info_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        child_name(_child_name_),
                                                        dostr(dynamic_context::dostr()),
                                                        child_info(_child_info_)
{
}


PPDebug::~PPDebug()
{
    delete child.op;
    child.op = NULL;
}

void PPDebug::open ()
{
    child.op->open();
    cc = 0;
}

void PPDebug::reopen ()
{
    child.op->reopen();
    cc = 0;
}

void PPDebug::close ()
{
    child.op->close();
}

void PPDebug::next(tuple &t)
{
    try
    {
        child.op->next(t);
        cc++;
    }
    catch(...)
    {
        dostr.set_debug_info_type(se_QueryDebug);

        dostr << child_name.get() << " : " << cc;

        if(child_info.get() != NULL) dostr << " : " << child_info.get();
        
        dostr << "\n";
        dostr.flush();

        /* 
        fprintf(stderr, "%15s", child_name.get());
        fprintf(stderr, " : %5d", &cc);
        
        child_info.get() != NULL ? 
            fprintf(stderr, " : %15s\n", child_info.get()) :
            fprintf(stderr, "\n"); 
        */
        
        throw;
    }
}

/// ??? Is there any specific behaviour in copy ???
PPIterator* PPDebug::copy(dynamic_context *_cxt_)
{
    PPDebug *res = child_info.get() != NULL ? 
                   new PPDebug(_cxt_, child, child_name, child_info) : 
                   new PPDebug(_cxt_, child, child_name);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPDebug::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPDebug::result");
}

