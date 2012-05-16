/*
 * File:  RQPVisitor.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _RQP_VISITOR_H_
#define _RQP_VISITOR_H_

#include "tr/opt/algebra/TupleScheme.h"
#include "tr/opt/algebra/PlanOperations.h"

namespace rqp {
  
class PlanVisitor {
  public:
    virtual ~PlanVisitor() { };

    virtual void visit(ConstantOperation * n) = 0;
    virtual void visit(ListOperation * n) = 0;
    virtual void visit(BinaryOperation * n) = 0;
    virtual void visit(NestedOperation * n) = 0;

//    virtual void visit(AtomicConstructor * n) = 0;
//    virtual void visit(GeneralComparison * n) = 0;
//    virtual void visit(Select * n) = 0;
//    virtual void visit(If * n) = 0;
//    virtual void visit(Let * n) = 0;
//    virtual void visit(Map * n) = 0;
//    virtual void visit(ItemReduce * n) = 0;

/*    
    virtual void visit(MapProduct * n) = 0;
    virtual void visit(SemiJoin * n) = 0;
    virtual void visit(Reduce * n) = 0;
    virtual void visit(Product * n) = 0;
    virtual void visit(Collect * n) = 0;
    virtual void visit(Enumerate * n) = 0;
*/    
};

}

#endif /* _RQP_VISITOR_H_ */
