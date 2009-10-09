/*
 * File:  ASTRevokePriv.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_REVOKE_PRIV_H_
#define _AST_REVOKE_PRIV_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTRevokePriv : public ASTNode
{
public:
    enum ObjMod
    {
        DOCUMENT,
        COLLECTION,
        DB
    };

    std::string *priv, *obj, *user; // obj will be NULL for DB-mod
    ObjMod mod;

public:
    ASTRevokePriv(const ASTNodeCommonData &loc, std::string *priv_, std::string *obj_, std::string *user_, ObjMod mod_) :
        ASTNode(loc),
        priv(priv_),
        obj(obj_),
        user(user_),
        mod(mod_)
    {}

    ~ASTRevokePriv();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
