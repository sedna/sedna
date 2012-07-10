#ifndef _VALUE_FUNCTION_H_
#define _VALUE_FUNCTION_H_

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/op_map.h"

class ValueFunction {
public:
    enum simple_function_t {
        atomized_value,
        nid_value,
        binary,
        atomized_binary,
        unary,
        atomized_unary,
        custom_function,
    };
protected:
    simple_function_t simple_type;

    struct binary_t {
        bin_op_tuple_cell_tuple_cell_collation binary;
        unsigned idxL;
        unsigned idxR;
    };

    struct unary_t {
        un_op_tuple_cell unary;
        unsigned idx;
    };
/*    
    struct unary_t {
        un_op_tuple_cell unary;
        unsigned idx;
    };
*/
    union {
        binary_t b;
        unary_t u;
//        custom_t;
    } params;

    tuple_cell do_evaluate(const tuple & x) { U_ASSERT(false); return tuple_cell(); };
public:
    CollationHandler * handler;

    explicit ValueFunction(bin_op_tuple_cell_tuple_cell_collation bin, unsigned idxL, unsigned idxR, bool atomized)
      : simple_type(atomized ? atomized_binary : binary)
    {
        params.b.binary = bin;
        params.b.idxL = idxL;
        params.b.idxR = idxR;
    };

    explicit ValueFunction(un_op_tuple_cell un, unsigned idx, bool atomized)
      : simple_type(atomized ? atomized_unary : unary)
    {
        params.u.unary = un;
        params.u.idx = idx;
    };

    explicit ValueFunction(unsigned idx, bool atomized)
      : simple_type(atomized ? atomized_value : nid_value) { params.u.idx = idx; }

    tuple_cell evaluate(const tuple & x) {
        switch(simple_type) {
          case atomized_value:
            return atomize(x.cells[params.u.idx]);
          case nid_value:
            return tuple_cell::eos();
          case binary:
            return params.b.binary(x.cells[params.b.idxL], x.cells[params.b.idxR], handler);
          case atomized_binary:
            return params.b.binary(atomize(x.cells[params.b.idxL]), atomize(x.cells[params.b.idxR]), handler);
          case unary:
            return params.u.unary(x.cells[params.u.idx]);
          case atomized_unary:
            return params.u.unary(atomize(x.cells[params.u.idx]));
          default:
            return do_evaluate(x);
        };
    };
};

#endif /* _VALUE_FUNCTION_H_ */
