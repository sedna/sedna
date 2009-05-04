#ifndef _AST_BASE_URI_H_
#define _AST_BASE_URI_H_

#include "ASTNode.h"
#include "AST.h"

class ASTBaseURI : public ASTNode
{
public:
    std::string *uri;

public:
    ASTBaseURI(ASTLocation &loc, std::string *base_uri) : ASTNode(loc), uri(base_uri) {}

    ~ASTBaseURI();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif
