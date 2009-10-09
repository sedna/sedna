#ifndef __XQ_COMMON_H__
#define __XQ_COMMON_H__

#include "tr/xqp/ast/ASTNode.h"
#include "tr/xqp/ast/ASTVarDecl.h"
#include "tr/xqp/XQFunction.h"
#include <string.h>
#include <map>

#define CREATE_INTNAME(u, l) ((u == "") ? (l) : (std::string("{") + (u) + std::string("}") + (l)))

namespace sedna
{
    struct XQVariable
    {
        std::string int_name;
        ASTVar *var;

        xqExprInfo exp_info;

        XQVariable(const char *name, ASTVar *var_)
        {
            int_name = name;
            var = var_;

            exp_info.isDistincted = true;
            exp_info.useConstructors = false;
            exp_info.isSingleLevel = true;
            exp_info.isOrdered = true;
            exp_info.isMax1 = true;
        }
    };

    typedef std::pair<std::string, ASTLocation *> nsPair;
    typedef std::map<std::string, nsPair> nsBindType; // location is used to diagnoze illegal redefinition
    typedef std::map<std::string, sedna::XQFunction> XQFunctionInfo;
    typedef std::map<std::string, ASTVarDecl *> XQVariablesInfo;
    typedef std::map<std::string, ASTNode *> XQStringHash;
}

#endif
