/*
 * File:  ASTAttrConst.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_ATTR_CONST_H_
#define _AST_ATTR_CONST_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTAttrConst : public ASTNode
{
public:
    ASTNode *name; // computed name, or
    std::string *pref, *local; // qualified name
    ASTNode *expr; // computed construction expression

    bool deep_copy; // attribute will be attached to virtual_root and copied on demand

public:
    ASTAttrConst(const ASTNodeCommonData &loc, ASTNode *name_, ASTNode *expr_ = NULL) : ASTNode(loc), name(name_), pref(NULL), local(NULL), expr(expr_) {}
    ASTAttrConst(const ASTNodeCommonData &loc, std::string *name_, ASTNode *expr_ = NULL) : ASTNode(loc), name(NULL), expr(expr_)
    {
        ASTParseQName(name_, &pref, &local);

        delete name_;
    }
    ASTAttrConst(const ASTNodeCommonData &loc, std::string *pref_, std::string *local_, ASTNode *expr_ = NULL) : ASTNode(loc), name(NULL), pref(pref_), local(local_), expr(expr_) {}

    ~ASTAttrConst();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
