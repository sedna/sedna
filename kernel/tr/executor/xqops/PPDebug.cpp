/*
 * File:  PPDebug.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"
#include "common/u/uutils.h"

#include "tr/executor/xqops/PPDebug.h"
#include "tr/tr_globals.h"


PPDebug::PPDebug(dynamic_context *_cxt_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_) : PPIterator(_cxt_),
                                                        child(_child_),
                                                        dostr(tr_globals::client->get_debug_ostream()),
                                                        child_name(_child_name_)
{
}

PPDebug::PPDebug(dynamic_context *_cxt_,
                 const PPOpIn &_child_,
                 const str_counted_ptr &_child_name_,
                 const str_counted_ptr &_child_info_) : PPIterator(_cxt_),
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
    SET_CURRENT_PP(this);
    
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

    RESTORE_CURRENT_PP;
}

/// FIXME!!! Is there any specific behaviour in copy (IS)?
PPIterator* PPDebug::copy(dynamic_context *_cxt_)
{
    PPDebug *res = child_info.get() != NULL ? 
                   se_new PPDebug(_cxt_, child, child_name, child_info) : 
                   se_new PPDebug(_cxt_, child, child_name);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPDebug::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPDebug::result");
}

