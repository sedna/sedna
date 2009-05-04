#ifndef _AST_NSP_H_
#define _AST_NSP_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTNsp : public ASTNode
{
public:
    std::string *name; // namespace name (NCName)
    ASTNodesVector *cont; // namespace content; may be NULL

public:
    ASTNsp(ASTLocation &loc, std::string *name_, ASTNodesVector *cont_ = NULL) : ASTNode(loc), name(name_), cont(cont_) {}

    ~ASTNsp();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
