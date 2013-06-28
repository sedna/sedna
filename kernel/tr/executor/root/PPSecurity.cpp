/*
 * File:  PPSecurity.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPSecurity.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/locks/locks.h"
#include "tr/auth/auc.h"

PPCreateUser::PPCreateUser(PPOpIn _username_,
                           PPOpIn _passwd_,
                           dynamic_context *_cxt_) : PPUpdate("PPCreateUser"),
                                                     username(_username_),
                                                     passwd(_passwd_),
                                                     cxt(_cxt_)
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

void PPCreateUser::do_open()
{
    cxt->global_variables_open();
    username.op->open();
    passwd.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPCreateUser::do_close()
{
    username.op->close();
    passwd.op->close();
    cxt->global_variables_close();
}

void PPCreateUser::do_execute()
{
    tuple_cell tc_user, tc_passwd;
    xqp_tuple t(1);

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


PPDropUser::PPDropUser(PPOpIn _username_,
                       dynamic_context *_cxt_) : PPUpdate("PPDropUser"),
                                                 username(_username_),
                                                 cxt(_cxt_)
{
}

PPDropUser::~PPDropUser()
{
    delete username.op;
    username.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropUser::do_open()
{
    cxt->global_variables_open();
    username.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPDropUser::do_close()
{
    username.op->close();
    cxt->global_variables_close();
}

void PPDropUser::do_execute()
{
    tuple_cell tc_user;
    xqp_tuple t(1);

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


PPAlterUser::PPAlterUser(PPOpIn _username_,
                         PPOpIn _passwd_,
                         dynamic_context *_cxt_) : PPUpdate("PPAlterUser"),
                                                   username(_username_),
                                                   passwd(_passwd_),
                                                   cxt(_cxt_)
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

void PPAlterUser::do_open()
{
    cxt->global_variables_open();
    username.op->open();
    passwd.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPAlterUser::do_close()
{
    username.op->close();
    passwd.op->close();
    cxt->global_variables_close();
}

void PPAlterUser::do_execute()
{
    tuple_cell tc_user, tc_passwd;
    xqp_tuple t(1);

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


PPCreateRole::PPCreateRole(PPOpIn _rolename_,
                           dynamic_context *_cxt_) : PPUpdate("PPCreateRole"),
                                                     rolename(_rolename_),
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

void PPCreateRole::do_open()
{
    cxt->global_variables_open();
    rolename.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPCreateRole::do_close()
{
    rolename.op->close();
    cxt->global_variables_close();
}

void PPCreateRole::do_execute()
{
    tuple_cell tc_role;
    xqp_tuple t(1);

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


PPDropRole::PPDropRole(PPOpIn _rolename_,
                       dynamic_context *_cxt_) : PPUpdate("PPDropRole"),
                                                 rolename(_rolename_),
                                                 cxt(_cxt_)
{
}

PPDropRole::~PPDropRole()
{
    delete rolename.op;
    rolename.op = NULL;

    delete cxt;
    cxt = NULL;
}

void PPDropRole::do_open()
{
    cxt->global_variables_open();
    rolename.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPDropRole::do_close()
{
    rolename.op->close();
    cxt->global_variables_close();
    clear_authmap();
}

void PPDropRole::do_execute()
{
    tuple_cell tc_role;
    xqp_tuple t(1);

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


PPGrantRole::PPGrantRole(PPOpIn _role_,
                         PPOpIn _grantee_,
                         dynamic_context *_cxt_) : PPUpdate("PPGrantRole"),
                                                   role(_role_),
                                                   grantee(_grantee_),
                                                   cxt(_cxt_)
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

void PPGrantRole::do_open()
{
    cxt->global_variables_open();
    role.op->open();
    grantee.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPGrantRole::do_close()
{
    role.op->close();
    grantee.op->close();
    cxt->global_variables_close();
    clear_authmap();
}

void PPGrantRole::do_execute()
{
    tuple_cell tc_role, tc_grantee;
    xqp_tuple t(1);

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


PPGrantRevokePriv::PPGrantRevokePriv(PPOpIn _name_,
                                     PPOpIn _obj_name_,
                                     PPOpIn _grantee_,
                                     const char *_obj_type_,
                                     dynamic_context *_cxt_,
                                     bool _revoke_) : PPUpdate("PPGrantRevokePriv"),
                                                      name(_name_),
                                                      obj_name(_obj_name_),
                                                      grantee(_grantee_),
                                                      obj_type(_obj_type_),
                                                      cxt(_cxt_),
                                                      to_revoke(_revoke_)
{
}

PPGrantRevokePriv::PPGrantRevokePriv(PPOpIn _name_,
                                     PPOpIn _grantee_,
                                     dynamic_context *_cxt_,
                                     bool _revoke_) : PPUpdate("PPGrantRevokePriv"),
                                                      name(_name_),
                                                      grantee(_grantee_),
                                                      cxt(_cxt_),
                                                      to_revoke(_revoke_)
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

void PPGrantRevokePriv::do_open()
{
    cxt->global_variables_open();
    name.op->open();

    if (obj_name.op)
        obj_name.op->open();

    grantee.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPGrantRevokePriv::do_close()
{
    name.op->close();

    if (obj_name.op)
        obj_name.op->close();

    grantee.op->close();
    cxt->global_variables_close();
    clear_authmap();
}

void PPGrantRevokePriv::do_execute()
{
    tuple_cell tc_name, tc_grantee;
    xqp_tuple t(1);

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


PPRevokeRole::PPRevokeRole(PPOpIn _role_,
                           PPOpIn _grantee_,
                           dynamic_context *_cxt_) : PPUpdate("PPRevokeRole"),
                                                     role(_role_),
                                                     grantee(_grantee_),
                                                     cxt(_cxt_)
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

void PPRevokeRole::do_open()
{
    cxt->global_variables_open();
    role.op->open();
    grantee.op->open();
    local_lock_mrg->lock(lm_x);
}

void PPRevokeRole::do_close()
{
    role.op->close();
    grantee.op->close();
    cxt->global_variables_close();
    clear_authmap();
}

void PPRevokeRole::do_execute()
{
    tuple_cell tc_role, tc_grantee;
    xqp_tuple t(1);

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

void PPCreateUser::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    username.op->accept(v);
    passwd.op->accept(v);
    v.pop();
}
void PPDropUser::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    username.op->accept(v);
    v.pop();
}
void PPAlterUser::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    username.op->accept(v);
    passwd.op->accept(v);
    v.pop();
}
void PPCreateRole::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    rolename.op->accept(v);
    v.pop();
}
void PPDropRole::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    rolename.op->accept(v);
    v.pop();
}
void PPGrantRole::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    role.op->accept(v);
    grantee.op->accept(v);
    v.pop();
}
void PPGrantRevokePriv::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    name.op->accept(v);
    if (obj_name.op)
        obj_name.op->accept(v);
    grantee.op->accept(v);
    v.pop();
}
void PPRevokeRole::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    role.op->accept(v);
    grantee.op->accept(v);
    v.pop();
}

