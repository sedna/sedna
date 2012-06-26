/*
 * File:  PlanOperationTypes.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PLAN_OP_MINIMAL_H_
#define _PLAN_OP_MINIMAL_H_

#include "common/sedna.h"

namespace rqp {
  
class PlanContext;

struct TupleDefinition;

class RPBase;

class ConstantOperation;
class NestedOperation;
class ListOperation;
class BinaryOperation;

static RPBase * const null_op = (RPBase *)(NULL);

/**** Logical operations ****/

class MapGraph;
class MapConcat;

};

#endif /* _PLAN_OP_MINIMAL_H_ */
