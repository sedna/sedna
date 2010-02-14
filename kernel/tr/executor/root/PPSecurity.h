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
private:
    PPOpIn username, passwd;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPCreateUser(PPOpIn _username_,
                 PPOpIn _passwd_,
                 dynamic_context *_cxt_);
    ~PPCreateUser();
};

class PPDropUser : public PPUpdate
{
private:
    PPOpIn username;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDropUser(PPOpIn _username_,
               dynamic_context *_cxt_);
    ~PPDropUser();
};

class PPAlterUser : public PPUpdate
{
private:
    PPOpIn username, passwd;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPAlterUser(PPOpIn _username_,
                PPOpIn _passwd_,
                dynamic_context *_cxt_);
    ~PPAlterUser();
};

class PPCreateRole : public PPUpdate
{
private:
    PPOpIn rolename;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPCreateRole(PPOpIn _rolename_,
                 dynamic_context *_cxt_);
    ~PPCreateRole();
};

class PPDropRole : public PPUpdate
{
private:
    PPOpIn rolename;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDropRole(PPOpIn _rolename_,
                 dynamic_context *_cxt_);
    ~PPDropRole();
};

class PPGrantRole : public PPUpdate
{
private:
    PPOpIn role, grantee;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPGrantRole(PPOpIn _role_,
                PPOpIn _grantee_,
                dynamic_context *_cxt_);
    ~PPGrantRole();
};

class PPGrantRevokePriv : public PPUpdate
{
private:
    PPOpIn name, obj_name, grantee;
    const char *obj_type;
    dynamic_context *cxt;
    bool to_revoke;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

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
    
    inline const char* get_object_type() { return obj_type; } 
};

class PPRevokeRole : public PPUpdate
{
private:
    PPOpIn role, grantee;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPRevokeRole(PPOpIn _role_,
                 PPOpIn _grantee_,
                 dynamic_context *_cxt_);
    ~PPRevokeRole();
};

#endif /* _PPSECURITY_H */
