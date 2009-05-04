#ifndef _AST_TYPE_VAR_H_
#define _AST_TYPE_VAR_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTTypeSeq;
class ASTVar;

class ASTTypeVar : public ASTNode
{
public:
    // types are mutually exclusive, either we've got a string such as "xs:anytype" or a full-fledged TypeSeq node
    std::string *type_name;
    ASTTypeSeq *type_seq;

    ASTVar *var;

public:
    ASTTypeVar(ASTLocation &loc, std::string *type, ASTVar *vard) : ASTNode(loc), type_name(type), type_seq(NULL), var(vard) {}
    ASTTypeVar(ASTLocation &loc, ASTTypeSeq *type, ASTVar *vard) : ASTNode(loc), type_name(NULL), type_seq(type), var(vard) {}

    ~ASTTypeVar();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
