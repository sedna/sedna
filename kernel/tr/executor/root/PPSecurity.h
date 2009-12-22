/*
 * File:  PPSecurity.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPSECURITY_H
#define _PPSECURITY_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPCreateUser : public PPUpdate
{
    PPOpIn username, passwd;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPCreateUser(PPOpIn _username_,
                 PPOpIn _passwd_,
                 dynamic_context *_cxt_);
    ~PPCreateUser();
};

class PPDropUser : public PPUpdate
{
    PPOpIn username;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPDropUser(PPOpIn _username_,
               dynamic_context *_cxt_);
    ~PPDropUser();
};

class PPAlterUser : public PPUpdate
{
    PPOpIn username, passwd;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPAlterUser(PPOpIn _username_,
                PPOpIn _passwd_,
                dynamic_context *_cxt_);
    ~PPAlterUser();
};

class PPCreateRole : public PPUpdate
{
    PPOpIn rolename;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPCreateRole(PPOpIn _rolename_,
                 dynamic_context *_cxt_);
    ~PPCreateRole();
};

class PPDropRole : public PPUpdate
{
    PPOpIn rolename;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPDropRole(PPOpIn _rolename_,
                 dynamic_context *_cxt_);
    ~PPDropRole();
};

class PPGrantRole : public PPUpdate
{
    PPOpIn role, grantee;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPGrantRole(PPOpIn _role_,
                PPOpIn _grantee_,
                dynamic_context *_cxt_);
    ~PPGrantRole();
};

class PPGrantRevokePriv : public PPUpdate
{
    PPOpIn name, obj_name, grantee;
    const char *obj_type;
    dynamic_context *cxt;
    bool to_revoke;

public:
    void open();
    void close();
    void execute();

    PPGrantRevokePriv(PPOpIn _name_,
                PPOpIn _obj_name_,
                PPOpIn _grantee_,
                const char *_obj_type_,
                dynamic_context *_cxt_,
                bool _revoke_);

    PPGrantRevokePriv(PPOpIn _name_,
                PPOpIn _grantee_,
                dynamic_context *_cxt_,
                bool _revoke_);

    ~PPGrantRevokePriv();
};

class PPRevokeRole : public PPUpdate
{
    PPOpIn role, grantee;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();

    PPRevokeRole(PPOpIn _role_,
                 PPOpIn _grantee_,
                 dynamic_context *_cxt_);
    ~PPRevokeRole();
};

#endif
