#ifndef __XQFUNCTION_H__
#define __XQFUNCTION_H__

#include <string>
#include "ast/ASTFuncDecl.h"

namespace sedna
{
    // param mask is a mask there each "1" bit tells us to take distinct-only param
    // NOTE: full mask (all "1"'s) means that every parameter might be distinct-only
    typedef unsigned char param_mask;
    const param_mask maxParamMask = 0xFF;

    struct xqExprInfo
    {
        bool isOrdered;     // expr is ordered
        bool isDistincted;  // expr contains distincted values
        bool isMax1;        // expr emits singleton or empty sequence
        bool isSingleLevel; // all nodes are on the same level in node-sequence
        bool useConstructors; // true, if subexpression uses constructor (direct or computed)
    };

    struct XQFunction
    {
        std::string uri;
        std::string local;

        unsigned int min_arg;
        unsigned int max_arg;

        param_mask mask;

        std::string int_name;

        bool toCache; // do we need to cache (PPStore) the result

        xqExprInfo exp_info; // we use it only for built-in functions

        ASTFuncDecl *decl;
        ASTLocation *loc;

        std::string mod_uri; // uri of library module, or "" if main or xquery
    };

    extern XQFunction xqueryFunctions[];
    extern XQFunction sqlFunctions[];
    extern XQFunction seFunctions[];
}

#endif
