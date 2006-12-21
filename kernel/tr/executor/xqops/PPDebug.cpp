/*
 * File:  PPDebug.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPDebug.h"


PPDebug::PPDebug(dynamic_context *_cxt_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        child_name(_child_name_)
{
}

PPDebug::PPDebug(dynamic_context *_cxt_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_,
                 const str_counted_ptr &_child_info_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        child_name(_child_name_),
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
        fprintf(stderr, "%15s", child_name.get());
        fprintf(stderr, " : %5d", &cc);
        
        child_info.get() != NULL ? 
            fprintf(stderr, " : %15s\n", child_info.get()) :
            fprintf(stderr, "\n");
        
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

