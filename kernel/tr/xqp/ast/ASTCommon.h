#ifndef _AST_COMMON_
#define _AST_COMMON_

#include <string>
#include <vector>
#include "tr/xqp/ast/ASTNode.h"

class ASTVisitor;
class ASTNode;

/* vector to store sequence of std::string nodes */
typedef std::vector<std::string *> ASTStringVector;

// Parses QName to prefix and local parts and stores them in pref and loc strings
// NOTE: function doesn't check corectness, if qname is 'pref:loc' then it is straightforward, else it is pref="", loc="qname"
//
// Parameters:
//      qname -- QName to parse
//      pref(ret) -- string to store prefix (or "")
//      loc(ret) -- string to store local part
//
// Returns:
//      pref and loc
void ASTParseQName(const std::string *qname, std::string **pref, std::string **loc);

/* helper to destroy vector of nodes and corresponding elements */
void destroyASTNodesVector(ASTNodesVector *nodes);

/* helper to destroy vector of strings and corresponding elements */
void destroyASTStringVector(ASTStringVector *strs);

/* duplicates nodes vector and its content */
ASTNodesVector *duplicateASTNodes(ASTNodesVector *nodes);

/* duplicates string vector and its content */
ASTStringVector *duplicateASTStringVector(ASTStringVector *strs);

#endif
