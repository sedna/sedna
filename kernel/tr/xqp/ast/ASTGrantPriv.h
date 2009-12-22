/*
 * File:  ASTGrantPriv.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_GRANT_PRIV_H_
#define _AST_GRANT_PRIV_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTGrantPriv : public ASTNode
{
public:
    enum ObjMod
    {
        DOCUMENT,
        COLLECTION,
        DB
    };

    std::string *priv, *obj, *user; // obj may be NULL (in case of DB)
    ObjMod mod;

public:
    ASTGrantPriv(const ASTNodeCommonData &loc, std::string *priv_, std::string *obj_, std::string *user_, ObjMod mod_) :
        ASTNode(loc),
        priv(priv_),
        obj(obj_),
        user(user_),
        mod(mod_)
    {}

    ~ASTGrantPriv();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
