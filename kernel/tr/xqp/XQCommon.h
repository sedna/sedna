#ifndef __XQ_COMMON_H__
#define __XQ_COMMON_H__

#include "tr/xqp/ast/ASTNode.h"
#include "tr/xqp/ast/ASTVarDecl.h"
#include "tr/xqp/XQFunction.h"
#include <string.h>
#include <map>

namespace sedna
{
    typedef std::pair<std::string, ASTLocation *> nsPair;
    typedef std::map<std::string, nsPair> nsBindType; // location is used to diagnoze illegal redefinition
    typedef std::map<std::string, sedna::XQFunction> XQFunctionInfo;
    typedef std::map<std::string, ASTVarDecl *> XQVariablesInfo;
    typedef std::pair<std::string, ASTVar *> XQVariable;
    typedef std::map<std::string, ASTNode *> XQStringHash;
}

#endif
