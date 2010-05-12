/*
 * File:  AST.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "ASTCommon.h"
#include <map>

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
void ASTParseQName(const std::string *qname, std::string **pref, std::string **loc)
{
    std::string::size_type pos;

    if (qname == NULL)
    {
        *pref = NULL;
        *loc = NULL;

        return;
    }

    if ((pos = qname->find(":")) != std::string::npos)
    {
        *pref = new std::string(qname->substr(0, pos));
        pos++;
    }
    else
    {
        *pref = new std::string("");
        pos = 0;
    }

    *loc = new std::string(qname->substr(pos, qname->size() - pos));
}

/* helper to destroy vector of nodes and corresponding elements */
void destroyASTNodesVector(ASTNodesVector *nodes)
{
    ASTNodesVector::iterator it;

    if (nodes == NULL) return;

    for (it = nodes->begin(); it != nodes->end(); it++)
        delete *it;

    delete nodes;
}

/* duplicates nodes vector and its content */
ASTNodesVector *duplicateASTNodes(ASTNodesVector *nodes)
{
    ASTNodesVector::const_iterator it;
    ASTNodesVector *res;

    if (nodes == NULL) return NULL;

    res = new ASTNodesVector();

    for (it = nodes->begin(); it != nodes->end(); it++)
        res->push_back((*it)->dup());

    return res;
}

/* duplicates string vector and its content */
ASTStringVector *duplicateASTStringVector(ASTStringVector *strs)
{
    ASTStringVector::const_iterator it;
    ASTStringVector *res;

    if (strs == NULL) return NULL;

    res = new ASTStringVector();

    for (it = strs->begin(); it != strs->end(); it++)
        res->push_back(new std::string(**it));

    return res;
}

/* helper to destroy string vector and corresponding elements */
void destroyASTStringVector(ASTStringVector *strs)
{
    ASTStringVector::iterator it;

    if (strs == NULL) return;

    for (it = strs->begin(); it != strs->end(); it++)
        delete *it;

    delete strs;
}
