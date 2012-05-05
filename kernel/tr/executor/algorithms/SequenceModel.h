/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_MODEL_H
#define SEQUENCE_MODEL_H

#include "SequenceHelpers.h"

#include "tr/executor/types/xmltuple.h"
#include "tr/opt/OptTypes.h"

#include <deque>

namespace phop {

class ExecutionContext;

class ITupleOperator {
public:
    unsigned tupleSize;
    std::deque<tuple> _values;
protected:
    ExecutionContext * _context;

    virtual void do_next() = 0;

    int _cacheSize;

    void seteos() { _values.back().set_eos(); };
    void push() { _values.push_back(tuple(tupleSize)); };
    tuple & value() { return _values.back(); };
public:
    ITupleOperator(unsigned _size) : tupleSize(_size), _context(NULL), _cacheSize(1) {};
    virtual ~ITupleOperator() {};

    virtual void reset() { _values.clear(); };

    inline bool next(ExecutionContext * context) {
        if (get().is_eos()) {
            return false;
        };

        if (_values.empty()) {
            _context = context;
            do_next();
        } else {
            _values.pop_front();
        };

        return true;
    };

    const tuple& get() const { return _values.front(); };
    unsigned _tsize() const { return tupleSize; };
};

struct TupleIn {
    ITupleOperator * op;
    unsigned offs;

    explicit TupleIn(ITupleOperator * _op, unsigned _offs) : op(_op), offs(_offs) {};

    ITupleOperator * operator->() const { return op; };

    bool eos() const { return op->get().is_eos(); };
    tuple_cell get() const { return op->get().cells[offs]; };

    void assignTo(tuple & result, const TupleMap & tmap) {
        if (tmap.size() > 0) {
            for (TupleMap::const_iterator it = tmap.begin(); it != tmap.end(); ++it) {
                result.cells[it->second] = op->get().cells[it->first];
            };
        }
    };
};

struct MappedTupleIn : public TupleIn {
    TupleMap tmap;

    explicit MappedTupleIn(ITupleOperator * _op, unsigned _offs, const TupleMap & _tmap)
        : TupleIn(_op, _offs), tmap(_tmap) { };

    explicit MappedTupleIn(ITupleOperator * _op, unsigned _offs, unsigned _result_offs)
        : TupleIn(_op, _offs)
    {
        tmap.reserve(_op->_tsize());
        for(unsigned i = 0; i < _op->_tsize(); ++i) { tmap.push_back(TupleMap::value_type(i, _result_offs + i)); }
    };

    explicit MappedTupleIn(ITupleOperator * _op, unsigned _offs)
        : TupleIn(_op, _offs), tmap() { };

    void assignTo(tuple & result) { assignTo(result, tmap); }
};

class BinaryTupleOperator : public ITupleOperator {
protected:
    MappedTupleIn left, right;
public:
    BinaryTupleOperator(unsigned _size, MappedTupleIn _left, MappedTupleIn _right)
        : ITupleOperator(_size), left(_left), right(_right) {};

    virtual void reset() { left.op->reset(); right.op->reset(); }; // TODO : move to implementation
};

class UnaryTupleOperator : public ITupleOperator {
protected:
    MappedTupleIn in;
public:
    UnaryTupleOperator(unsigned _size, MappedTupleIn _in) : ITupleOperator(_size), in(_in) {};

    virtual void reset() { in.op->reset(); };
};


}

#endif /* SEQUENCE_MODEL_H */
