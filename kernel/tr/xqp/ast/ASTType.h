/*
 * File:  ASTType.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_TYPE_H_
#define _AST_TYPE_H_

#include "ASTNode.h"
class ASTVisitor;

#include <string>

class ASTType : public ASTNode
{
public:
    enum TypeMod
    {
        ATOMIC = 0, // xs:integer, xs:string etc. AND xs:anyAtomicType
        ABSTRACT,   // xs:anyType, xs:anySimpleType
        COMPLEX,    // xs:untyped
        LIST,       // xs:NMTOKENS, xs:IDREFS etc.
        ANY,        // any type in hierarchy
    };

public:
    std::string *name; // QName for type
    TypeMod type;

public:
    ASTType(const ASTNodeCommonData &loc, std::string *type_, TypeMod mod = ASTType::ANY) : ASTNode(loc), name(type_), type(mod) {}

    ~ASTType();

    void accept(ASTVisitor &v);
    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
