#ifndef _AST_DESER_
#define _AST_DESER_

#include "tr/xqp/ast/ASTNode.h"
#include "tr/executor/por2qep/scheme_tree.h"

typedef ASTNode *(*createNode_fun)(scheme_list &sl);

ASTNode *dsGetASTFromString(const char *mod);

ASTNode *dsGetASTFromSchemeList(scheme_list &sl);

/* retrireves location from scheme_list (serialize-deserialize AST logic)
    location is stored in SCM_LIST as four SCM_NUMBER */
ASTNodeCommonData dsGetASTCommonFromSList(scheme_list &slist);

/* builds AST nodes vector from string; calls dsGetASTNodesFromSList */
ASTNodesVector *dsGetASTNodesFromString(const char *str);

/* builds AST nodes vector from the corresponding scheme_list; the format is: SCM_LIST(SCM_LIST(SCM_SYMBOL(node name), SCM_LIST(params)), ...) */
ASTNodesVector *dsGetASTNodesFromSList(scheme_list &slist);

/* builds AST strings vector from the corresponding scheme_list; the format is: SCM_LIST(SCM_LIST(SCM_SYMBOL(node name), SCM_LIST(params)), ...) */
ASTStringVector *dsGetASTStringsFromSList(scheme_list &slist);

#endif
