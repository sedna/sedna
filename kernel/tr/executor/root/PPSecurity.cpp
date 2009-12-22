/*
 * File:  PPSecurity.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/root/PPSecurity.h"
#include "tr/executor/base/PPUtils.h"
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

PPCreateRole::PPCreateRole(PPOpIn _rolename_, dynamic_context *_cxt_) : rolename(_rolename_), cxt(_cxt_)
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
