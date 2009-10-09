/*
 * File:  ASTOption.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_OPTION_H_
#define _AST_OPTION_H_

#include "ASTNode.h"
#include "AST.h"

class ASTOption : public ASTNode
{
public:
    std::string *pref, *local, *opt;

    // added at semantic analysis
    std::string *uri;

    typedef std::pair<std::string, std::string> option;
    std::vector<option> *options; // declare option se:output "indent=yes"; -> options["indent"] = "yes"

public:
    ASTOption(const ASTNodeCommonData &loc, std::string *pref_p, std::string *loc_p, std::string *option) : ASTNode(loc), pref(pref_p), local(loc_p), opt(option)
    {
        uri = NULL;
        options = NULL;
    }

    ASTOption(const ASTNodeCommonData &loc, std::string *qname, std::string *option) : ASTNode(loc), opt(option)
    {
        ASTParseQName(qname, &pref, &local);
        delete qname;

        uri = NULL;
        options = NULL;
    }

    ~ASTOption();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);
};

#endif
