#ifndef _AST_TYPE_SINGLE_H_
#define _AST_TYPE_SINGLE_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTAtomicTest;

class ASTTypeSingle : public ASTNode
{
public:
    enum OccurMod
    {
        ONE = 0,
        OPT,
    };

    ASTAtomicTest *type;

    OccurMod mod; // '?' or nothing (ONE)

public:
    ASTTypeSingle(ASTLocation &loc, ASTAtomicTest *type_, ASTTypeSingle::OccurMod omod = ONE) : ASTNode(loc), type(type_), mod(omod) {}

    ~ASTTypeSingle();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
