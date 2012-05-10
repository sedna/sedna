/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_MODEL_H
#define SEQUENCE_MODEL_H

#include "SequenceHelpers.h"
#include "tr/opt/OptTypes.h"

#include <deque>

namespace phop {

class ExecutionContext;

class IValueOperator {
    std::deque<tuple_cell> valueCache;
protected:
    ExecutionContext * _context;
    
    void seteos() { valueCache.push_back(tuple_cell()); };
    void push(const tuple_cell & tc) { valueCache.push_back(tc); };

    virtual void do_next() = 0;

    IValueOperator();
public:
    virtual ~IValueOperator() {};

    virtual void reset() { valueCache.clear(); };
    
    inline bool next(ExecutionContext * context) {
        if (get().is_eos()) {
            return false;
        };

        if (valueCache.empty()) {
            _context = context;
            do_next();
        } else {
            valueCache.pop_front();
        };

        return true;
    };
    
    const tuple_cell& get() const { return valueCache.front(); };
};

class ITupleOperator {
    tuple _value;
    IValueOperator * _convert_op;
protected:
    ExecutionContext * _context;

    void seteos() { _value.set_eos(); };
    tuple & value() { return _value; };

    virtual void do_next() = 0;
    
    ITupleOperator(IValueOperator * __convert_op)
      : _value(1), _convert_op(__convert_op)
    {
    };

    ITupleOperator(unsigned _size)
      : _value(_size), _convert_op(NULL), _context(NULL)
    {
        _value.eos = false;
    };
public:
    virtual ~ITupleOperator() {};

    virtual void reset() { _value.eos = false; };

    inline bool next(ExecutionContext * context) {
        if (_convert_op != NULL) {
            _convert_op->next(context);
            _value.copy(_convert_op->get());
        };

        if (get().is_eos()) {
            return false;
        };

        _context = context;
        do_next();

        return true;
    };

    const tuple& get() const { return _value; };
    unsigned _tsize() const { return _value.size(); };
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

    void assignTo(tuple & result) { TupleIn::assignTo(result, tmap); }
};

class TupleFromItemOperator : public ITupleOperator {
protected:
    virtual void do_next() { U_ASSERT(false); };
public:
    TupleFromItemOperator(IValueOperator* convert_op)
      : ITupleOperator(convert_op) {};
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

class ItemOperator : public IValueOperator {
protected:
    IValueOperator * in;
public:
    ItemOperator(IValueOperator * _in) : IValueOperator(), in(_in) {};

    virtual void reset() { in->reset(); };
};

}

#endif /* SEQUENCE_MODEL_H */
