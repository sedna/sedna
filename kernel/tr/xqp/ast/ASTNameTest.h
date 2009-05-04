#ifndef _AST_NAME_TEST_H_
#define _AST_NAME_TEST_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTNameTest : public ASTNode
{
public:
    std::string *pref, *local;

public:
    ASTNameTest(ASTLocation &loc, std::string *name) : ASTNode(loc)
    {
        ASTParseQName(name, &pref, &local);
    }

    ASTNameTest(ASTLocation &loc, std::string *pref_, std::string *local_) : ASTNode(loc), pref(pref_), local(local_) {}

    ~ASTNameTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
