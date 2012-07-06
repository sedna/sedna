#ifndef _ELEMENTARY_OPERATIONS_H_
#define _ELEMENTARY_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {

struct EmptySequenceConst { explicit EmptySequenceConst(int x = 0) {}; };

class Const : public ConstantOperation {
    OPERATION(0x012)
    opt::MemoryTupleSequencePtr sequence;
public:
    explicit Const(opt::MemoryTupleSequencePtr _sequence)
      : ConstantOperation(&sopdesc), sequence(_sequence)
    {};

    explicit Const(tuple_cell _value)
      : ConstantOperation(&sopdesc), sequence(new opt::MemoryTupleSequence())
    { sequence->push_back(_value); };

    explicit Const(const EmptySequenceConst &)
      : ConstantOperation(&sopdesc), sequence(new opt::MemoryTupleSequence())
    { };

    PROPERTY_RO(Sequence, opt::MemoryTupleSequencePtr, sequence)
};

class VarIn : public ConstantOperation {
    OPERATION(0x011)
private:
    opt::TupleId tid;

    void setDataNode(opt::TupleId _tid);
public:
    /* Phantom datanode */
    opt::DataNode * dnode;

    VarIn(opt::TupleId _tid)
      : ConstantOperation(&sopdesc), tid(_tid)
    {
        setDataNode(_tid);
    };

    PROPERTY_RO(Tuple, opt::TupleId, tid)
};

class Exists : public ListOperation {
    OPERATION(0x01d)
public:
    Exists(RPBase * list)
      : ListOperation(&sopdesc, list) {};
};

class Sequence : public ManyChildren {
    OPERATION(0x019)
public:
    enum space_t {
        none,
        atomic_spaces,
        all_spaces
    };

private:
    space_t spaces;
public:
    Sequence(const OperationList & _oplist)
      : ManyChildren(&sopdesc, _oplist), spaces(none) { };

    Sequence(RPBase* _in)
      : ManyChildren(&sopdesc, _in), spaces(none) { };

    PROPERTY(Spaces, space_t, spaces)
};

}

#endif /* _ELEMENTARY_OPERATIONS_H_ */
