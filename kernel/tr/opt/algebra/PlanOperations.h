/*
 * File:  PlanOperations.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PLAN_OP_MINIMAL_H_
#define _PLAN_OP_MINIMAL_H_

#include "common/sedna.h"
#include "tr/opt/OptForewards.h"

namespace rqp {

struct TupleDefinition;

enum op_list_t {
  op_base,

  op_nil,
  op_uni,
  op_nested,
  op_multi,

  op_const,
  op_varin,
  op_exists, // Important: differs from effective boolean value
  op_sequence,

  op_map,
  op_sequence_map,
  op_map_graph,

  op_if,
  op_funcall,

  op_xpath, // TODO : will be a fun call
  op_constructor, 

  op_datagraph, // TODO : delete
};

};

#endif /* _PLAN_OP_MINIMAL_H_ */
