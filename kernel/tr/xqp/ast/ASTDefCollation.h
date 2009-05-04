#ifndef _AST_DEF_COLLATION_H_
#define _AST_DEF_COLLATION_H_

#include "ASTNode.h"
#include "AST.h"

class ASTDefCollation : public ASTNode
{
public:
    std::string *uri;

public:
    ASTDefCollation(ASTLocation &loc, std::string *coll_uri) : ASTNode(loc), uri(coll_uri) {}

    ~ASTDefCollation();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif
