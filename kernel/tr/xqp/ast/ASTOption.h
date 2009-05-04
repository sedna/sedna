#ifndef _AST_OPTION_H_
#define _AST_OPTION_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOption : public ASTNode
{
public:
    std::string *pref, *local, *opt;

public:
    ASTOption(ASTLocation &loc, std::string *pref_p, std::string *loc_p, std::string *option) : ASTNode(loc), pref(pref_p), local(loc_p), opt(option) {}

    ASTOption(ASTLocation &loc, std::string *qname, std::string *option) : ASTNode(loc), opt(option)
    {
        ASTParseQName(qname, &pref, &local);
    }

    ~ASTOption();

    void accept(ASTVisitor &v);

    ASTNode *dup();
};

#endif
