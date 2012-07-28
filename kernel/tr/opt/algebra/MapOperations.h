#ifndef _MAP_OPERATIONS_H_
#define _MAP_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

/* 1r-operations with independent nested operation plan */
class NestedOperation : public ListOperation {
  RTTI_DECL(plan_operation_NestedOperation, ListOperation)
protected:
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    /* Phantom datanode */
    opt::DataNode * dnode;
    void setDataNode(opt::TupleId _tid);

    NestedOperation(clsinfo_t op, RPBase * list_, RPBase * subplan_, opt::TupleId _tid)
      : ListOperation(op, list_) {
        children.push_back(subplan_);
        setDataNode(_tid);
    };

    PROPERTY(Subplan, RPBase *, children[1])

    opt::TupleId tuple() const { return dnode->varTupleId; };
};
  
/*
 * MapConcat operation binds the context variable
 *
 * Operation returns the result of _plan_
 * for each element form subplan sequence
 */

class MapConcat : public NestedOperation {
    RTTI_DECL(plan_operation_MapConcat, NestedOperation)
public:
    MapConcat(RPBase* _list, RPBase* _subplan, const ContextInfo & _context)
      : NestedOperation(SELF_RTTI_REF, _list, _subplan, _context.item)
    {
        resultChild = 0;
    };

    MapConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _context)
      : NestedOperation(SELF_RTTI_REF, _list, _subplan, _context)
    {
        resultChild = 0;
    };
};

class SequenceConcat : public NestedOperation {
    RTTI_DECL(plan_operation_SequenceConcat, NestedOperation)
public:
    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(SELF_RTTI_REF, _list, _subplan, _tid)
    {
        resultChild = 0;
    };
};

}

#endif /* _MAP_OPERATIONS_H_ */
