/*
 * File:  PPDebug.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"
#include "common/u/uutils.h"

#include "tr/executor/xqops/PPDebug.h"
#include "tr/tr_globals.h"
#include "tr/executor/base/visitor/PPVisitor.h"


PPDebug::PPDebug(dynamic_context *_cxt_,
                 operation_info _info_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_) : PPIterator(_cxt_, _info_),
                                                        child(_child_),
                                                        dostr(tr_globals::client->get_debug_ostream()),
                                                        child_name(_child_name_)
{
}

PPDebug::PPDebug(dynamic_context *_cxt_,
                 operation_info _info_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_,
                 const str_counted_ptr &_child_info_) : PPIterator(_cxt_, _info_),
                                                        child(_child_),
                                                        dostr(tr_globals::client->get_debug_ostream()),
                                                        child_name(_child_name_),
                                                        child_info(_child_info_)
{
}


PPDebug::~PPDebug()
{
    delete child.op;
    child.op = NULL;
}

void PPDebug::do_open ()
{
    child.op->open();
    cc = 0;
}

void PPDebug::do_reopen()
{
    child.op->reopen();
    cc = 0;
}

void PPDebug::do_close()
{
    child.op->close();
}

void PPDebug::do_next(tuple &t)
{
    try
    {
        cc++;
        child.op->next(t);
    }
    catch(ANY_SE_EXCEPTION)
    {
        std::string message;
        char str[40];

        message += child_name.get();
        message +=  " : ";
        message += u_itoa(cc, str, 10);

        if(child_info.get() != NULL) 
        {    
            message += " : "; 
            message += child_info.get();
        }
        
        message += "\n";
        
        dostr->set_debug_info_type(se_QueryDebug);
        (*dostr) << message.c_str();
        dostr->flush();

        throw;
    }
}

PPIterator* PPDebug::do_copy(dynamic_context *_cxt_)
{
    PPDebug *res = child_info.get() != NULL ? 
                   se_new PPDebug(_cxt_, info, child, child_name, child_info) : 
                   se_new PPDebug(_cxt_, info, child, child_name);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPDebug::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);
    v.pop();
}
