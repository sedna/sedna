/*
 * File:  PPSecurity.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPSecurity.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"

PPCreateUser::PPCreateUser(PPOpIn _username_, PPOpIn _passwd_, dynamic_context *_cxt_) : username(_username_), passwd(_passwd_), cxt(_cxt_)
{
}

PPCreateUser::~PPCreateUser()
{
    delete username.op;
    username.op = NULL;

    delete passwd.op;
    passwd.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPCreateUser::open()
{
    dynamic_context::global_variables_open();
    username.op->open();
    passwd.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPCreateUser::close()
{
    username.op->close();
    passwd.op->close();
    dynamic_context::global_variables_close();
}

void PPCreateUser::execute()
{
    tuple_cell tc_user, tc_passwd;
    tuple t(1);

    username.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_user = username.get(t);
    if (!tc_user.is_atomic() || tc_user.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    username.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_user = tuple_cell::make_sure_light_atomic(tc_user);

    passwd.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_passwd = passwd.get(t);
    if (!tc_passwd.is_atomic() || tc_passwd.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    passwd.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_passwd = tuple_cell::make_sure_light_atomic(tc_passwd);

    auth_for_create_user(tc_user.get_str_mem(), tc_passwd.get_str_mem());
}

// PPDropUser

PPDropUser::PPDropUser(PPOpIn _username_, dynamic_context *_cxt_) : username(_username_), cxt(_cxt_)
{
}

PPDropUser::~PPDropUser()
{
    delete username.op;
    username.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropUser::open()
{
    dynamic_context::global_variables_open();
    username.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPDropUser::close()
{
    username.op->close();
    dynamic_context::global_variables_close();
}

void PPDropUser::execute()
{
    tuple_cell tc_user;
    tuple t(1);

    username.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_user = username.get(t);
    if (!tc_user.is_atomic() || tc_user.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    username.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_user = tuple_cell::make_sure_light_atomic(tc_user);

    auth_for_drop_user(tc_user.get_str_mem());
}

// PPAlterUser

PPAlterUser::PPAlterUser(PPOpIn _username_, PPOpIn _passwd_, dynamic_context *_cxt_) : username(_username_), passwd(_passwd_), cxt(_cxt_)
{
}

PPAlterUser::~PPAlterUser()
{
    delete username.op;
    username.op = NULL;

    delete passwd.op;
    passwd.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPAlterUser::open()
{
    dynamic_context::global_variables_open();
    username.op->open();
    passwd.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPAlterUser::close()
{
    username.op->close();
    passwd.op->close();
    dynamic_context::global_variables_close();
}

void PPAlterUser::execute()
{
    tuple_cell tc_user, tc_passwd;
    tuple t(1);

    username.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_user = username.get(t);
    if (!tc_user.is_atomic() || tc_user.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    username.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_user = tuple_cell::make_sure_light_atomic(tc_user);

    passwd.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_passwd = passwd.get(t);
    if (!tc_passwd.is_atomic() || tc_passwd.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    passwd.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_passwd = tuple_cell::make_sure_light_atomic(tc_passwd);

    auth_for_alter_user(tc_user.get_str_mem(), tc_passwd.get_str_mem());
}

// PPCreateRole

PPCreateRole::PPCreateRole(PPOpIn _rolename_, dynamic_context *_cxt_) : rolename(_rolename_), 
                                                                        cxt(_cxt_)
{
}

PPCreateRole::~PPCreateRole()
{
    delete rolename.op;
    rolename.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPCreateRole::open()
{
    dynamic_context::global_variables_open();
    rolename.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPCreateRole::close()
{
    rolename.op->close();
    dynamic_context::global_variables_close();
}

void PPCreateRole::execute()
{
    tuple_cell tc_role;
    tuple t(1);

    rolename.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = rolename.get(t);
    if (!tc_role.is_atomic() || tc_role.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    rolename.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = tuple_cell::make_sure_light_atomic(tc_role);

    auth_for_create_role(tc_role.get_str_mem());
}

// PPDropRole

PPDropRole::PPDropRole(PPOpIn _rolename_, dynamic_context *_cxt_) : rolename(_rolename_), cxt(_cxt_)
{
}

PPDropRole::~PPDropRole()
{
    delete rolename.op;
    rolename.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropRole::open()
{
    dynamic_context::global_variables_open();
    rolename.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPDropRole::close()
{
    rolename.op->close();
    dynamic_context::global_variables_close();
    clear_authmap();
}

void PPDropRole::execute()
{
    tuple_cell tc_role;
    tuple t(1);

    rolename.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = rolename.get(t);
    if (!tc_role.is_atomic() || tc_role.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    rolename.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = tuple_cell::make_sure_light_atomic(tc_role);

    auth_for_drop_role(tc_role.get_str_mem());
}

// PPGrantRole

PPGrantRole::PPGrantRole(PPOpIn _role_, PPOpIn _grantee_, dynamic_context *_cxt_) : role(_role_), grantee(_grantee_), cxt(_cxt_)
{
}

PPGrantRole::~PPGrantRole()
{
    delete role.op;
    role.op = NULL;

    delete grantee.op;
    grantee.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPGrantRole::open()
{
    dynamic_context::global_variables_open();
    role.op->open();
    grantee.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPGrantRole::close()
{
    role.op->close();
    grantee.op->close();
    dynamic_context::global_variables_close();
    clear_authmap();
}

void PPGrantRole::execute()
{
    tuple_cell tc_role, tc_grantee;
    tuple t(1);

    role.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = role.get(t);
    if (!tc_role.is_atomic() || tc_role.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    role.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = tuple_cell::make_sure_light_atomic(tc_role);

    grantee.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_grantee = grantee.get(t);
    if (!tc_grantee.is_atomic() || tc_grantee.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    grantee.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_grantee = tuple_cell::make_sure_light_atomic(tc_grantee);

    auth_for_grant_role(tc_role.get_str_mem(), tc_grantee.get_str_mem());
}

// PPGrantRole

PPGrantRevokePriv::PPGrantRevokePriv(PPOpIn _name_,
                         PPOpIn _obj_name_,
                         PPOpIn _grantee_,
                         const char *_obj_type_,
                         dynamic_context *_cxt_,
                         bool _revoke_) : name(_name_), obj_name(_obj_name_), grantee(_grantee_), obj_type(_obj_type_), cxt(_cxt_), to_revoke(_revoke_)
{
}

PPGrantRevokePriv::PPGrantRevokePriv(PPOpIn _name_,
                         PPOpIn _grantee_,
                         dynamic_context *_cxt_,
                         bool _revoke_) : name(_name_), grantee(_grantee_), cxt(_cxt_), to_revoke(_revoke_)
{
    obj_name.op = NULL;
    obj_type = "db";
}

PPGrantRevokePriv::~PPGrantRevokePriv()
{
    delete name.op;
    name.op = NULL;

    delete grantee.op;
    grantee.op = NULL;

    delete obj_name.op;
    obj_name.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPGrantRevokePriv::open()
{
    dynamic_context::global_variables_open();
    name.op->open();

    if (obj_name.op)
        obj_name.op->open();

    grantee.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPGrantRevokePriv::close()
{
    name.op->close();

    if (obj_name.op)
        obj_name.op->close();

    grantee.op->close();
    dynamic_context::global_variables_close();
    clear_authmap();
}

void PPGrantRevokePriv::execute()
{
    tuple_cell tc_name, tc_grantee;
    tuple t(1);

    name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_name = name.get(t);
    if (!tc_name.is_atomic() || tc_name.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_name = tuple_cell::make_sure_light_atomic(tc_name);

    grantee.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_grantee = grantee.get(t);
    if (!tc_grantee.is_atomic() || tc_grantee.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    grantee.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_grantee = tuple_cell::make_sure_light_atomic(tc_grantee);

    if (obj_name.op)
    {
        tuple_cell tc_obj;

        obj_name.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION(SE1071);

        tc_obj = obj_name.get(t);
        if (!tc_obj.is_atomic() || tc_obj.get_atomic_type() != xs_string)
            throw USER_EXCEPTION(SE1071);

        obj_name.op->next(t);
        if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

        tc_obj = tuple_cell::make_sure_light_atomic(tc_obj);

        if (to_revoke)
            auth_for_revoke_privilege(tc_name.get_str_mem(), tc_obj.get_str_mem(), obj_type, tc_grantee.get_str_mem());
        else
            auth_for_grant_privilege(tc_name.get_str_mem(), tc_obj.get_str_mem(), obj_type, tc_grantee.get_str_mem());
    }
    else
    {
        if (to_revoke)
            auth_for_revoke_privilege(tc_name.get_str_mem(), NULL, NULL, tc_grantee.get_str_mem());
        else
            auth_for_grant_privilege(tc_name.get_str_mem(), NULL, NULL, tc_grantee.get_str_mem());
    }
}

// PPRevokeRole

PPRevokeRole::PPRevokeRole(PPOpIn _role_, PPOpIn _grantee_, dynamic_context *_cxt_) : role(_role_), grantee(_grantee_), cxt(_cxt_)
{
}

PPRevokeRole::~PPRevokeRole()
{
    delete role.op;
    role.op = NULL;

    delete grantee.op;
    grantee.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPRevokeRole::open()
{
    dynamic_context::global_variables_open();
    role.op->open();
    grantee.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPRevokeRole::close()
{
    role.op->close();
    grantee.op->close();
    dynamic_context::global_variables_close();
    clear_authmap();
}

void PPRevokeRole::execute()
{
    tuple_cell tc_role, tc_grantee;
    tuple t(1);

    role.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = role.get(t);
    if (!tc_role.is_atomic() || tc_role.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    role.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_role = tuple_cell::make_sure_light_atomic(tc_role);

    grantee.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_grantee = grantee.get(t);
    if (!tc_grantee.is_atomic() || tc_grantee.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    grantee.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc_grantee = tuple_cell::make_sure_light_atomic(tc_grantee);

    auth_for_revoke_role(tc_role.get_str_mem(), tc_grantee.get_str_mem());
}


////////////////////////////////////////////////////////////////////////////////
/// Acceptors for security root operations
////////////////////////////////////////////////////////////////////////////////

void PPCreateUser::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    username.op->accept(v);
    passwd.op->accept(v);
    v.pop();
}
void PPDropUser::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    username.op->accept(v);
    v.pop();
}
void PPAlterUser::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    username.op->accept(v);
    passwd.op->accept(v);
    v.pop();
}
void PPCreateRole::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    rolename.op->accept(v);
    v.pop();
}
void PPDropRole::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    rolename.op->accept(v);
    v.pop();
}
void PPGrantRole::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    role.op->accept(v);
    grantee.op->accept(v);
    v.pop();
}
void PPGrantRevokePriv::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    name.op->accept(v);
    if (obj_name.op)
        obj_name.op->accept(v);
    grantee.op->accept(v);
    v.pop();
}
void PPRevokeRole::accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    role.op->accept(v);
    grantee.op->accept(v);
    v.pop();
}

