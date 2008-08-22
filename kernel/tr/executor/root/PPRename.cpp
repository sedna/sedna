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
                   const char *_ncname_local_): PPUpdate(),
                                             child(_child_),
                                             cxt(_cxt_),
                                             ncname_prefix(_ncname_prefix_),
                                             ncname_local(_ncname_local_),
                                             type(PP_RENAME_NODE)
{
}

PPRename::PPRename(PPOpIn _child_, 
                   PPOpIn _new_name_child_, 
                   dynamic_context *_cxt_) : PPUpdate(),
                                             child(_child_),
                                             new_name_child(_new_name_child_),
                                             cxt(_cxt_),
                                             type(PP_RENAME_COLLECTION)
{
}


PPRename::~PPRename()
{
    delete child.op;
    child.op = NULL;
    if(new_name_child.op)
    {
        delete new_name_child.op;
        new_name_child.op = NULL;
    }
    delete cxt;
    cxt = NULL;
}

void PPRename::open()
{
    local_lock_mrg->lock(lm_x);
    dynamic_context::global_variables_open();
    child.op->open();
    if(new_name_child.op)
    {
        new_name_child.op->open();
    }
}

void PPRename::close()
{
    child.op->close();
    if(new_name_child.op)
    {
        new_name_child.op->close();
    }
    dynamic_context::global_variables_close();
}

void PPRename::execute()
{
    switch(type)
    {
        case(PP_RENAME_NODE): 
        {
            rename(child, ncname_local); 
            break;
        }
        
        case(PP_RENAME_COLLECTION):
        {
            tuple t(child.ts);
            child.op->next(t);
            if (t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Empty old-name is given in rename collection.");
            tuple_cell old_name = atomize(child.get(t));
            if(!is_string_type(old_name.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, "Unexpected type of the old-name argument in rename collection (xs:string/derived/promotable is expected).");
            child.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the old-name argument in rename collection. Argument contains more than one item.");

            new_name_child.op->next(t);
            if (t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Empty new-name is given in rename collection.");
            tuple_cell new_name = atomize(child.get(t));
            if(!is_string_type(new_name.get_atomic_type())) 
                throw USER_EXCEPTION2(XPTY0004, "Unexpected type of the new-name argument in rename collection (xs:string/derived/promotable is expected).");
            new_name_child.op->next(t);
            if (!t.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the new name-argument in rename collection. Argument contains more than one item.");

            local_lock_mrg->put_lock_on_collection(old_name.get_str_mem());
            local_lock_mrg->put_lock_on_collection(new_name.get_str_mem());

            auth_for_rename_collection(old_name.get_str_mem(), new_name.get_str_mem());

            rename_collection(old_name.get_str_mem(), new_name.get_str_mem());
            break;
        }

        default: 
            throw USER_EXCEPTION2(SE1003, "Unexpected type in update collection operation.");
    }
}

