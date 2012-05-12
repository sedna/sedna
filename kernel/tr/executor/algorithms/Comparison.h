#ifndef _COMPARISON_H_
#define _COMPARISON_H_

#include "tr/executor/base/ITupleSerializer.h"
#include "tr/executor/fo/op_map.h"
#include "tr/executor/base/PPUtils.h"

class CollationHandler;

tuple_cell op_doc_order_lt (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_doc_order_gt (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);

tuple_cell op_doc_order_descendant (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);
tuple_cell op_doc_order_ancestor (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler);

struct TupleCellComparison {
    bin_op_tuple_cell_tuple_cell_collation lessop;
    bin_op_tuple_cell_tuple_cell_collation predop;

    // TODO : delete this
    bool generalComparison;

    CollationHandler * handler;

    TupleCellComparison(
        bin_op_tuple_cell_tuple_cell_collation _lessop,
        bin_op_tuple_cell_tuple_cell_collation _predop,
        bool gcmp
    )
      : lessop(_lessop), predop(_predop), generalComparison(gcmp), handler(NULL) {};

    bool less(const tuple_cell & a, const tuple_cell & b) {
        if (generalComparison) {
            return lessop(atomize(a), atomize(b), handler).get_xs_boolean();
        } else {
            return lessop(a, b, handler).get_xs_boolean();
        };
    };

    bool satisfy(const tuple_cell & a, const tuple_cell & b) {
        if (generalComparison) {
            return predop(atomize(a), atomize(b), handler).get_xs_boolean();
        } else {
            return predop(a, b, handler).get_xs_boolean();
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

#endif /* _COMPARISON_H_ */
