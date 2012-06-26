#ifndef _MAP_OPERATIONS_H_
#define _MAP_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {
  
class MapConcat : public NestedOperation {
    OPERATION(0x002)
public:
    ContextInfo context;

    MapConcat(RPBase* _list, RPBase* _subplan, const ContextInfo & _context)
      : NestedOperation(&sopdesc, _list, _subplan), context(_context) {};
};

class SequenceConcat : public NestedOperation {
    OPERATION(0x003)
public:
    opt::TupleId tid;

    SequenceConcat(RPBase* _list, RPBase* _subplan, opt::TupleId _tid)
      : NestedOperation(&sopdesc, _list, _subplan), tid(_tid) {};
};

/*
 * Select operation binds the context variable
 */

class Select : public NestedOperation {
    OPERATION(0x004)
public:
    ContextInfo context;

    Select(RPBase* _list, RPBase* _subplan, const ContextInfo & _context)
      : NestedOperation(&sopdesc, _list, _subplan), context(_context) {};
};

}

#endif /* _MAP_OPERATIONS_H_ */
