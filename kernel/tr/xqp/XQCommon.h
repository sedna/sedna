#ifndef __XQ_COMMON_H__
#define __XQ_COMMON_H__

#include "tr/xqp/ast/ASTNode.h"
#include "tr/xqp/ast/ASTVarDecl.h"
#include "tr/xqp/ast/ASTVar.h"
#include <string.h>
#include <map>

#define CREATE_INTNAME(u, l) ((u == "") ? (l) : (std::string("{") + (u) + std::string("}") + (l)))
#define CREATE_INTNAME_FUN(u, l, a) (CREATE_INTNAME(u, l) + std::string("/") + int2string(a))

namespace sedna
{
    class XQFunction;
    struct xqExprInfo
    {
        bool isOrdered;     // expr is ordered
        bool isDistincted;  // expr contains distincted values
        bool isMax1;        // expr emits singleton or empty sequence
        bool isSingleLevel; // all nodes are on the same level in node-sequence
        bool useConstructors; // true, if subexpression uses constructor (direct or computed)
    };

    typedef int var_id;

    struct XQVariable
    {
        std::string int_name;
        ASTVar *var;

        xqExprInfo exp_info;

        bool isNodes; // true if var represents sequence of nodes (singletons also go here)(this is only for typed vars)

        var_id id; // id for physical plan

        XQVariable(const char *name, ASTVar *var_)
        {
            int_name = name;
            var = var_;
            id = -1;

            exp_info.isDistincted = true;
            exp_info.useConstructors = false;
            exp_info.isSingleLevel = true;
            exp_info.isOrdered = true;
            exp_info.isMax1 = true;
        }

        XQVariable()
        {
            int_name = "$%dummy";
            var = NULL;
            id = -1;
        }
    };

    typedef std::pair<std::string, ASTLocation *> nsPair;
    typedef std::map<std::string, nsPair> nsBindType; // location is used to diagnoze illegal redefinition
    typedef std::map<std::string, XQFunction> XQFunctionInfo;
    typedef std::map<std::string, ASTVarDecl *> XQVariablesInfo;
    typedef std::map<std::string, ASTNode *> XQStringHash;
}

#endif
