/*
 * File:  PPRename.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPRENAME_H
#define _PPRENAME_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/XPath.h"

class PPRename : public PPUpdate
{
public:
    enum pp_rename_type {
        PP_RENAME_NODE,
        PP_RENAME_COLLECTION,
    };
    
    static inline const char* pp_rename_type2c_string(pp_rename_type type)
    {
        switch(type)
        {
        case PP_RENAME_NODE: return "node";
        case PP_RENAME_COLLECTION: return "collection";
        default: throw USER_EXCEPTION2(SE1003, "Impossible type in rename type to string conversion");
        }
    }
    
private:
    
    PPOpIn child;
    PPOpIn new_name_child;
    dynamic_context *cxt;
    xsd::NCName ncname_prefix;
    xsd::NCName ncname_local;
    pp_rename_type type;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    PPRename(PPOpIn _child_, 
             dynamic_context *_cxt_,
             xsd::NCName _ncname_prefix_,
             xsd::NCName _ncname_local_);

    PPRename(PPOpIn _child_, 
             PPOpIn _new_name_child_,
             dynamic_context *_cxt_);

    ~PPRename();
     
};


#endif

