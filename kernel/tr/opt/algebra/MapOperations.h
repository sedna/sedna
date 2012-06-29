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
      : NestedOperation(&sopdesc, _list, _subplan, _context.item) {};
};

class SequenceConcat : public NestedOperation {
    OPERATION(0x003)
public:
    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(&sopdesc, _list, _subplan, _tid) {};
};

/*
 * Select operation binds the context variable
 *
 * Operation returns the result of _subplan_
 * if EBV of evaluation of _list_ is true for
 * each element from the _subplan_ sequence
 */

class Select : public NestedOperation {
    OPERATION(0x004)
public:
    Select(RPBase* _list, RPBase* _subplan, const ContextInfo & _context)
      : NestedOperation(&sopdesc, _list, _subplan, _context.item) {};
};

}

#endif /* _MAP_OPERATIONS_H_ */
