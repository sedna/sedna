#ifndef __XQ_COMMON_H__
#define __XQ_COMMON_H__

#include "tr/xqp/ast/ASTNode.h"
#include "tr/xqp/ast/ASTVarDecl.h"
#include "tr/xqp/ast/ASTVar.h"
#include "tr/executor/base/dynamic_context.h"
#include <string.h>
#include <map>

#define CREATE_INTNAME(u, l) ((u == "") ? (l) : (std::string("{") + (u) + std::string("}") + (l)))
#define CREATE_INTNAME_FUN(u, l, a) (CREATE_INTNAME(u, l) + std::string("/") + cast_to_string<int>(a))

namespace sedna
{
    struct xqExprInfo
    {
        bool isOrdered;     // expr is ordered
        bool isDistincted;  // expr contains distincted values
        bool isMax1;        // expr emits singleton or empty sequence
        bool isSingleLevel; // all nodes are on the same level in node-sequence
        bool useConstructors; // true, if subexpression uses constructor (direct or computed)
    };

    class XQueryModule;

    /*
     * Variable info: used throughout the parsing-qep process
     */
    struct XQVariable
    {
        // internal info
        std::string int_name;
        XQueryModule *mod;

        ASTNode *var;

        // lreturn info
        xqExprInfo exp_info;
        bool isNodes; // true if var represents sequence of nodes (singletons also go here)(this is only for typed vars)

        // lr2por info
        global_var_dsc id; // id for physical plan

        // true if variable is actually being used
        bool is_used;

        XQVariable(const char *name, ASTNode *var_, XQueryModule *mod_ = NULL)
        {
            int_name = name;
            mod = mod_;
            var = var_;
            isNodes = false;

            exp_info.isDistincted = true;
            exp_info.useConstructors = false;
            exp_info.isSingleLevel = true;
            exp_info.isOrdered = true;
            exp_info.isMax1 = true;

            id = global_var_dsc((dynamic_context *)NULL, INVALID_VAR_DSC);
            is_used = false;
        }

        XQVariable()
        {
            int_name = "$%dummy";
            mod = NULL;
            var = NULL;
            id = global_var_dsc((dynamic_context *)NULL, INVALID_VAR_DSC);
            is_used = false;
        }
    };

    struct XQFunction;

    typedef std::pair<std::string, ASTLocation *> nsPair;
    typedef std::map<std::string, nsPair> nsBindType; // location is used to diagnoze illegal redefinition
    typedef std::map<std::string, XQFunction *> XQFunctionInfo;
    typedef std::map<std::string, XQFunction> XQStdFunctionInfo;
    typedef std::map<std::string, XQVariable *> XQVariablesInfo;
    typedef std::map<std::string, ASTNode *> XQStringHash;
}

#endif
