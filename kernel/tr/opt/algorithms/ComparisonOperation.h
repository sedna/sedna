#ifndef _COMPARISON_OPERATION_H_
#define _COMPARISON_OPERATION_H_

#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/base/tuple.h"
#include "tr/executor/fo/op_map.h"
#include "tr/executor/base/PPUtils.h"

class CollationHandler;

tuple_cell op_doc_order_lt (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_doc_order_gt (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);

tuple_cell op_doc_order_descendant (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_doc_order_ancestor (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);

tuple_cell op_doc_order_descendant_or_self (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_doc_order_ancestor_or_self (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);

struct TupleCellComparison {
    enum {
        opt_none = 0,
        opt_swap = 0x1,
        opt_atomize = 0x2,
        opt_swap_atomize = 0x3,
    };

    bin_op_tuple_cell_tuple_cell_collation lessop;
    bin_op_tuple_cell_tuple_cell_collation predop;

    int opts; // TODO : delete this
    CollationHandler * handler;

    TupleCellComparison(
        bin_op_tuple_cell_tuple_cell_collation _lessop,
        bin_op_tuple_cell_tuple_cell_collation _predop,
        bool gcmp,
        CollationHandler * _handler
    )
      : lessop(_lessop), predop(_predop), opts(gcmp ? opt_atomize : 0), handler(_handler) {};

    TupleCellComparison inverse()
    {
        TupleCellComparison result(*this);
        result.opts ^= opt_swap; // xor with swap
        return result;
    };

    bool less(const tuple_cell & a, const tuple_cell & b) {
        switch (opts) {
          case opt_none:
            return lessop(a, b, handler).get_xs_boolean();
          case opt_swap:
            return lessop(b, a, handler).get_xs_boolean();
          case opt_atomize:
            return lessop(atomize(a), atomize(b), handler).get_xs_boolean();
          case opt_swap_atomize:
            return lessop(atomize(b), atomize(a), handler).get_xs_boolean();
          default: U_ASSERT(false); return false;
        };
    };

    bool satisfy(const tuple_cell & a, const tuple_cell & b) {
        switch (opts) {
          case opt_none:
            return predop(a, b, handler).get_xs_boolean();
          case opt_swap:
            return predop(b, a, handler).get_xs_boolean();
          case opt_atomize:
            return predop(atomize(a), atomize(b), handler).get_xs_boolean();
          case opt_swap_atomize:
            return predop(atomize(b), atomize(a), handler).get_xs_boolean();
          default: U_ASSERT(false); return false;
        };
    };
};

class ICollationTupleSerializer : public ITupleSerializer {
protected:
    CollationHandler * collation;
public:
    void setCollationHandler(CollationHandler * _collation) { collation = _collation; } ;
};

class GeneralCollationSerializer : public ICollationTupleSerializer {
    unsigned idx;
public:
    GeneralCollationSerializer(unsigned _idx) : idx(_idx) {};

    virtual size_t serialize(const tuple& t, void* buf);
    virtual void deserialize(tuple& t, void* buf, size_t size);
    virtual int compare(void* buf1, size_t size1, void* buf2, size_t size2);
};

class DocOrderSerializer : public ICollationTupleSerializer {
    unsigned idx;
public:
    DocOrderSerializer(unsigned _idx) : idx(_idx) {};

    virtual size_t serialize(const tuple& t, void* buf);
    virtual void deserialize(tuple& t, void* buf, size_t size);
    virtual int compare(void* buf1, size_t size1, void* buf2, size_t size2);
};

#endif /* _COMPARISON_OPERATION_H_ */
