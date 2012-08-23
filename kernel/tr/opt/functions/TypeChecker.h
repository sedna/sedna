#ifndef _TYPE_CHECKER_H_
#define _TYPE_CHECKER_H_

#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/opt/OptTypes.h"

struct error_info_t {
    int code;
    const char * description;
};

template <typename SequenceType, typename TypeChecker>
struct TypedSequenceIterator
{
    SequenceType iterator;
    TypeChecker checker;
    int min, max;
    error_info_t error_info;
    tuple_cell value;

    TypedSequenceIterator(
        const SequenceType & _iterator,
        const TypeChecker & _checker,
        int _min, int _max,
        const error_info_t & _error_info)

      : iterator(_iterator),
        checker(_checker),
        min(_min), max(-_max),
        error_info(_error_info) {}

    inline
    tuple_cell next()
    {
        value = iterator.next();

        if (value.is_eos()) {
            if (min > 0) {
                throw USER_EXCEPTION2(error_info.code, error_info.description);
            };

            return value;
        }

        if (!checker.check(value)) {
            throw USER_EXCEPTION2(error_info.code, error_info.description);
        };

        min--;
        max++;

        if (max == 0) {
            if (!iterator.next().is_eos()) {
                throw USER_EXCEPTION2(error_info.code, error_info.description);
            }
        };

        return value;
    };
};

template <typename IteratorType>
struct StlIterator
{
    IteratorType tc, tend;

    StlIterator(const IteratorType & start, const IteratorType & end)
      : tc(start), tend(end) {};

    inline
    tuple_cell next()
    {
        if (tc == tend) {
            return tuple_cell();
        };

        return *(tc++);
    };
};

typedef StlIterator<opt::MemoryTupleSequence::iterator> MemoryTuplesWrapper;

struct AnyTypeChecker
{
    AnyTypeChecker() {};
    inline bool check(tuple_cell & tc) { return true; };
};

struct AtomicTypeChecker
{
    xmlscm_type type;
    AtomicTypeChecker(xmlscm_type _type) : type(_type) {};

    inline
    bool check(tuple_cell & tc)
    {
        return tc.is_atomic_type(type);
    };
};

struct LightAtomicTypeChecker
{
    xmlscm_type type;
    LightAtomicTypeChecker(xmlscm_type _type) : type(_type) {};

    inline
    bool check(tuple_cell & tc)
    {
        if (tc.is_atomic_type(type)) {
            tc = tuple_cell::make_sure_light_atomic(tc);
            return true;
        }

        return false;
    };
};

struct NodeChecker
{
    int type_mask;
    NodeChecker(int _type_mask) : type_mask(_type_mask) {};

    inline
    bool check(tuple_cell & tc)
    {
        // TODO : add node type check
        return tc.is_node();
    };
};

struct AtomicTypeCaster
{
    xmlscm_type type;
    AtomicTypeCaster(xmlscm_type _type) : type(_type) {};

    inline
    bool check(tuple_cell & tc)
    {
        if (!tc.is_atomic_type(type)) {
            tc = cast(atomize(tc), type);
        };

        tc = tuple_cell::make_sure_light_atomic(tc);

        return tc.is_atomic_type(type);
    };
};

struct TypeAtomizer
{
    TypeAtomizer() {};

    inline
    bool check(tuple_cell & tc)
    {
        if (!tc.is_atomic()) {
            tc = atomize(tc);
        };

        return tc.is_atomic();
    };
};

#endif /* _TYPE_CHECKER_H_ */
