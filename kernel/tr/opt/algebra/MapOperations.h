#ifndef _MAP_OPERATIONS_H_
#define _MAP_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

/* 1r-operations with independent nested operation plan */
class NestedOperation : public RPBase {
  RTTI_DECL(plan_operation_NestedOperation, RPBase)
protected:
    opt::TupleId variableId;
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    NestedOperation(clsinfo_t op, RPBase * list_, RPBase * subplan_, opt::TupleId _tid)
      : RPBase(op), variableId(_tid) {
        children.push_back(subplan_);
        children.push_back(list_);
    };

    PROPERTY(Subplan, RPBase *, children[0])
    PROPERTY(List, RPBase *, children[1])
    PROPERTY(Tuple, opt::TupleId, variableId)
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
    MapConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _context)
      : NestedOperation(SELF_RTTI_REF, _list, _subplan, _context)
    {
        opt::TupleInfo & tinfo = context->varGraph.addVariableDeclaration(_context, this);
        tinfo.properties.flags = opt::tuple_info_t::sf_singleton;
        resultChild = 0;
    };
};

class SequenceConcat : public NestedOperation {
    RTTI_DECL(plan_operation_SequenceConcat, NestedOperation)
public:
    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(SELF_RTTI_REF, _list, _subplan, _tid)
    {
        context->varGraph.addVariableDeclaration(_tid, this);
        resultChild = 0;
    };
};

class GroupBy : public ListOperation {
    RTTI_DECL(plan_operation_GroupBy, ListOperation)
protected:
    opt::TupleId variableId;
    virtual XmlConstructor& __toXML(XmlConstructor& constructor ) const;
public:
    opt::tuple_info_t tmp_tinfo;

    GroupBy(RPBase* _list, opt::TupleId _context)
      : ListOperation(SELF_RTTI_REF, _list), variableId(_context)
    {
        opt::TupleInfo & tinfo = context->varGraph.addVariableUsage(_context, this);
        tmp_tinfo = tinfo.properties;
        resultChild = 0;
    };

    PROPERTY(Tuple, opt::TupleId, variableId)
};


}

#endif /* _MAP_OPERATIONS_H_ */
