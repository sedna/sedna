#ifndef _AST_TYPE_SEQ_H_
#define _AST_TYPE_SEQ_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTTypeSeq : public ASTNode
{
public:
    enum OccurMod
    {
        ONE = 0,
        OPT,
        ZERO_OR_MORE,
        ONE_OR_MORE
    };

    // types are mutually exclusive, either we've got a string such as "xs:integer" (atomic type) or some kind of test
    std::string *type_name;
    ASTNode *type_test;

    OccurMod mod; // '?', '*', '+'

public:
    ASTTypeSeq(ASTLocation &loc, std::string *type, ASTTypeSeq::OccurMod omod = ONE) : ASTNode(loc), type_name(type), type_test(NULL), mod(omod) {}
    ASTTypeSeq(ASTLocation &loc, ASTNode *type, ASTTypeSeq::OccurMod omod = ONE) : ASTNode(loc), type_name(NULL), type_test(type), mod(omod) {}

    ~ASTTypeSeq();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
