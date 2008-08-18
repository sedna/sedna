/*
 * File:  PPRename.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPRename.h"
#include "tr/updates/updates.h"
#include "tr/locks/locks.h"
#include "tr/structures/metadata.h"
#include "tr/auth/auc.h"

PPRename::PPRename(PPOpIn _child_, 
                   dynamic_context *_cxt_,
                   const char *_ncname_prefix_,
                   const char *_ncname_local_,
                   pp_rename_type _type_) : PPUpdate(),
                                            child(_child_),
                                            cxt(_cxt_),
                                            ncname_prefix(_ncname_prefix_),
                                            ncname_local(_ncname_local_),
                                            type(_type_)
{
}

PPRename::~PPRename()
{
    delete child.op;
    child.op = NULL;
    delete cxt;
    cxt = NULL;
}

void PPRename::open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child.op->open();
}

void PPRename::close()
{
    child.op->close();
    dynamic_context::global_variables_close();
}

void PPRename::execute()
{
    switch(type)
    {
        case(PP_RENAME_NODE): rename(child, ncname_local); break;
        case(PP_RENAME_COLLECTION):
        {
            tuple t(child.ts);
            child.op->next(t);
            if (t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Empty name is given in update rename collection.");
            tuple_cell old_name = atomize(child.get(t));
            if(!is_string_type(old_name.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, "Unexpected type of the name argument in update rename collection (xs:string/derived/promotable is expected).");
            child.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the name argument in update rename collection. Argument contains more than one item.");

            auth_for_rename_collection(old_name.get_str_mem(), ncname_local);
            
            local_lock_mrg->put_lock_on_collection(old_name.get_str_mem());
            local_lock_mrg->put_lock_on_collection(ncname_local);

            rename_collection(old_name.get_str_mem(), ncname_local);
            break;
        }
        default: throw USER_EXCEPTION2(SE1003, "Unexpected type in update collection operation.");
    }
}

