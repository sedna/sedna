/*
 * File: XQFunction.h
 * The Institute for System Programming of the Russian Academy of Sciences
 * Copyright (C) 2013 ISP RAS
 */

#ifndef __XQFUNCTION_H__
#define __XQFUNCTION_H__

#include <string>
#include "tr/xqp/ast/ASTFuncDecl.h"
#include "tr/xqp/XQCommon.h"
#include "tr/executor/base/PPBase.h"

namespace sedna
{
    // param mask is a mask there each "1" bit tells us to take distinct-only param
    // NOTE: full mask (all "1"'s) means that every parameter can be distinct-only
    typedef unsigned char param_mask;
    const param_mask maxParamMask = 0xFF;

    typedef xqExprInfo (*resultFunc)(const std::vector<xqExprInfo> &params);
    typedef PPOpIn (*l2pFunc)(dynamic_context *dyn_cxt, const operation_info &opi, arr_of_PPOpIn &params);

    /*
     * Function info: used throughout the parsing-qep process
     */
    struct XQFunction
    {
        std::string uri;
        std::string local;

        // min and max number of args
        unsigned int min_arg;
        unsigned int max_arg;

        param_mask mask; // parameter mask: 1 in i-position means: i-attribute needs distinct-only

        std::string int_name; // internal name: fn:doc <--> !fn!document

        bool toCache; // do we need to cache (PPStore) the result

        /*
         * merger allows to obtain result specs from params specs for internal functions.
         * Used on lreturn phase.
         */
        resultFunc merger;

        l2pFunc l2pGen; // this function generates qep representation for the function
        xqExprInfo exp_info; // we use it for user-defined functions (it is based on type-body analysis in lreturn)

        XQueryModule *mod; // module reference
        ASTFuncDecl *decl; // parsed declaration reference
        ASTLocation *loc;  // location

        std::string mod_uri; // uri of library module, or "" if main or xquery

        function_id id; // function id for qep-plan

        bool is_used; // true if function is actually being used

        // returns 'true' if the function is external one
        bool is_external() const
        {
            return (decl && !decl->body);
        }
    };

    extern XQFunction xqueryFunctions[];
    extern XQFunction sqlFunctions[];
    extern XQFunction seFunctions[];
    extern XQFunction cryptoFunctions[];
}

#endif
