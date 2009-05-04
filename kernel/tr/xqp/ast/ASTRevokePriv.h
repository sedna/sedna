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

    std::string *priv, *obj, *user;
    ObjMod mod;

public:
    ASTRevokePriv(ASTLocation &loc, std::string *priv_, std::string *obj_, std::string *user_, ObjMod mod_) :
        ASTNode(loc),
        priv(priv_),
        obj(obj_),
        user(user_),
        mod(mod_)
    {}

    ~ASTRevokePriv();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
