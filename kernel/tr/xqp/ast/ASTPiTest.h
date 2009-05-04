#ifndef _AST_PI_TEST_H_
#define _AST_PI_TEST_H_

#include "ASTNode.h"
#include "AST.h"

class ASTPiTest : public ASTNode
{
public:
    enum TypeTest
    {
        NCNAME,
        STRING,
        NONE
    };

    std::string *test;
    TypeTest type;

public:
    ASTPiTest(ASTLocation &loc, std::string *test_, TypeTest type_) : ASTNode(loc), test(test_), type(type_) {}
    ASTPiTest(ASTLocation &loc) : ASTNode(loc), test(NULL), type(NONE) {}

    ~ASTPiTest();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
