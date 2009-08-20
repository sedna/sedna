#ifndef __XQFUNCTION_H__
#define __XQFUNCTION_H__

#include <string>
#include "ast/ASTFuncDecl.h"

namespace sedna
{
    struct XQFunction
    {
        std::string uri;
        std::string local;

        unsigned int min_arg;
        unsigned int max_arg;

        std::string int_name;

        ASTFuncDecl *decl;
        ASTLocation *loc;

        std::string mod_uri; // uri of library module, or "" if main or xquery
    };

    extern XQFunction xqueryFunctions[];
    extern XQFunction sqlFunctions[];
    extern XQFunction seFunctions[];
}

#endif
