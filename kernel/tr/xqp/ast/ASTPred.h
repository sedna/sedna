/*
 * File:  ASTPred.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_PRED_H_
#define _AST_PRED_H_

#include "ASTNode.h"
class ASTVisitor;

#include "tr/executor/xqops/PPPred.h"

class ASTPred : public ASTNode
{
public:
    struct ASTConjunct
    {
        ASTNode *expr;
        operation_compare_condition op; // for "true" conjuncts
        bool use_last;
        bool use_pos;
        bool use_cxt;
    };

    typedef std::vector <ASTConjunct> ASTConjuncts;

public:
    ASTConjuncts conjuncts, others;

private:
    ASTPred(const ASTNodeCommonData &loc) : ASTNode(loc) {}

public:
    ASTPred(const ASTNodeCommonData &loc, ASTNode *expr_);

    ~ASTPred();

    void accept(ASTVisitor &v);

    ASTNode *dup();
    void modifyChild(const ASTNode *oldc, ASTNode *newc);

    static ASTNode *createNode(scheme_list &sl);

    void separateConjuncts(ASTNode *expr);
    void seriliazeConjuncts(std::string &str, ASTVisitor &v) const;

    bool usePosition() const;
    bool useLast() const;

    const ASTConjuncts &getConjuncts() const
    {
        return this->conjuncts;
    }
};

#endif
