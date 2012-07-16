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
    OPERATION(0x002)
public:
    MapConcat(RPBase* _list, RPBase* _subplan, const ContextInfo & _context)
      : NestedOperation(&sopdesc, _list, _subplan, _context.item)
    {
        resultChild = 0;
    };
};

class SequenceConcat : public NestedOperation {
    OPERATION(0x003)
public:
    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(&sopdesc, _list, _subplan, _tid)
    {
        resultChild = 0;
    };
};

}

#endif /* _MAP_OPERATIONS_H_ */
