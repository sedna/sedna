#ifndef _AST_PRAGMA_H_
#define _AST_PRAGMA_H_

#include "ASTNode.h"
#include "AST.h"

#include <string>

class ASTPragma : public ASTNode
{
public:
    std::string *pref, *local; // pragma name
    std::string *cont; // pragma content

public:
    ASTPragma(ASTLocation &loc, std::string *pragma_name, std::string *pragma_cont);

    ASTPragma(ASTLocation &loc, std::string *pr_pref, std::string *pr_local, std::string *pragma_cont);

    ~ASTPragma();

    void accept(ASTVisitor &v);
    ASTNode *dup();
};

#endif
