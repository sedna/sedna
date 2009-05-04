#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTFunCall.h"

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *func_name, ASTNodesVector *func_params) : ASTNode(loc), params(func_params)
{
    ASTParseQName(func_name, &pref, &local);
}

ASTFunCall::ASTFunCall(ASTLocation &loc, std::string *fun_pref, std::string *fun_local, ASTNodesVector *func_params)
        : ASTNode(loc),
          pref(fun_pref),
          local(fun_local),
          params(func_params)
{
}

ASTFunCall::~ASTFunCall()
{
    delete pref;
    delete local;
    destroyASTNodesVector(params);
}

void ASTFunCall::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTFunCall::dup()
{
    return new ASTFunCall(loc, new std::string(*pref), new std::string(*local), duplicateASTNodes(params));
}
