#include "tr/xqp/visitor/ASTVisitor.h"
#include "ASTCreateTrg.h"

ASTCreateTrg::~ASTCreateTrg()
{
    delete name;
    delete path;
    destroyASTNodesVector(do_exprs);
}

void ASTCreateTrg::accept(ASTVisitor &v)
{
    v.visit(*this);
}

ASTNode *ASTCreateTrg::dup()
{
    return new ASTCreateTrg(loc, new std::string(*name), t_mod, a_mod, path->dup(), g_mod, duplicateASTNodes(do_exprs));
}
