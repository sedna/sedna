#ifndef _MAP_OPERATIONS_H_
#define _MAP_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {

/*
 * MapConcat operation binds the context variable
 *
 * Operation returns the result of _plan_
 * for each element form subplan sequence
 */

class MapConcat : public NestedOperation {
    RTTI_DECL(plan_operation_MapConcat, NestedOperation)
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
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
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
public:
    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(SELF_RTTI_REF, _list, _subplan, _tid)
    {
        resultChild = 0;
    };
};

}

#endif /* _MAP_OPERATIONS_H_ */
