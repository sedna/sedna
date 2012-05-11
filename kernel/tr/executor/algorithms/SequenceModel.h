/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_MODEL_H
#define SEQUENCE_MODEL_H

#include "SequenceHelpers.h"
#include "tr/opt/OptTypes.h"

#include <deque>
#include <stack>

class POProt;

namespace phop {

class ExecutionContext;
class IOperator;
class ITupleOperator;

class ExecutionBlock {
    static std::stack<ExecutionBlock * > blockStack;
public:
    static ExecutionBlock * current()
    {
        return blockStack.top();
    };

    static ExecutionBlock * push(ExecutionBlock * executionBlock)
    {
        ExecutionBlock * result = blockStack.empty() ? NULL : blockStack.top();
        blockStack.push(executionBlock);
        return result;
    };

    static ExecutionBlock * pop()
    {
        ExecutionBlock * result = blockStack.top();
        blockStack.pop();
        return result;
    };
private:
    friend class IOperator;

    std::vector<IOperator *> body;
public:
    std::stack<POProt *> sourceStack;

    ExecutionBlock();
    ~ExecutionBlock();

    ExecutionBlock * copy();

    ITupleOperator * top()
    {
        return (ITupleOperator *)(body.back());
    };
};

struct operation_info_t {
    const char * name;
    int id;
};

#define OPINFO_T const phop::operation_info_t * 
#define OPINFO_DECL(ID) \
  static const struct phop::operation_info_t op_info; \
  static const int opid = ID; \
  virtual phop::IOperator * clone() const;

#define OPINFO_DEF(TT) \
  const struct phop::operation_info_t TT::op_info = {#TT, TT::opid}; \
  phop::IOperator * TT::clone() const { return new TT(*this); };

#define OPINFO_REF &op_info

class IOperator {
private:
    const operation_info_t * opinfo;
protected:
    ExecutionContext * _context;

    IOperator(OPINFO_T _opinfo);

    virtual void do_next() = 0;
public:
    virtual ~IOperator();
    virtual void reset() = 0;
    virtual IOperator * clone() const = 0;
    virtual void setContext(ExecutionContext * __context)
      { _context = __context; };

    const operation_info_t * info() const { return opinfo; };
};

class IValueOperator : public IOperator {
    std::deque<tuple_cell> valueCache;
protected:
    void seteos() { valueCache.push_back(tuple_cell()); };
    void push(const tuple_cell & tc) { valueCache.push_back(tc); };

    IValueOperator(OPINFO_T _opinfo) : IOperator(_opinfo) {};
public:
    virtual void reset()
      { valueCache.clear(); };

    inline bool next()
    {
        if (get().is_eos()) {
            return false;
        };

        if (valueCache.empty()) {
            do_next();
        } else {
            valueCache.pop_front();
        };

        return true;
    };
    
    const tuple_cell& get() const { return valueCache.front(); };
};

class ITupleOperator : public IOperator {
    tuple _value;
protected:
    IValueOperator * _convert_op;

    void seteos() { _value.set_eos(); };
    tuple & value() { return _value; };

    virtual void do_next() = 0;
    
    ITupleOperator(OPINFO_T _opinfo, IValueOperator * __convert_op);

    ITupleOperator(OPINFO_T _opinfo, unsigned _size) :
      IOperator(_opinfo), _value(_size), _convert_op(NULL) {};
public:
    virtual void reset()
      { _value.eos = false; };

    inline bool next() {
        if (_convert_op != NULL) {
            _convert_op->next();
            _value.copy(_convert_op->get());
        };

        if (get().is_eos()) {
            return false;
        };

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

    void assignTo(tuple & result, const TupleMap & tmap) const {
        if (tmap.size() > 0) {
            for (TupleMap::const_iterator it = tmap.begin(); it != tmap.end(); ++it) {
                result.cells[it->second] = op->get().cells[it->first];
            };
        }
    };

    void copyTo(tuple & result) const {
        result.copy(op->get());
    };
};

struct MappedTupleIn : public TupleIn {
    TupleMap tmap;

    explicit MappedTupleIn(const TupleIn & tin)
        : TupleIn(tin)
    {
        tmap.reserve(op->_tsize());
        for(unsigned i = 0; i < op->_tsize(); ++i) { tmap.push_back(TupleMap::value_type(i, i)); }
    };

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

    void assignTo(tuple & result) const { TupleIn::assignTo(result, tmap); }
};

class TupleFromItemOperator : public ITupleOperator {
protected:
    virtual void do_next();
public:
    OPINFO_DECL(0x001)

    TupleFromItemOperator(IValueOperator* convert_op);

    virtual void reset();
    virtual void setContext(ExecutionContext* __context);
};

class ReduceToItemOperator : public IValueOperator {
protected:
    TupleIn in;

    virtual void do_next();
public:
    OPINFO_DECL(0x002)

    ReduceToItemOperator(const TupleIn & op);

    virtual void reset();
    virtual void setContext(ExecutionContext* __context);
};

class BinaryTupleOperator : public ITupleOperator {
protected:
    MappedTupleIn left, right;

    BinaryTupleOperator(OPINFO_T _opinfo, unsigned _size, const MappedTupleIn & _left, const MappedTupleIn & _right)
        : ITupleOperator(_opinfo, _size), left(_left), right(_right) {};
public:
    virtual void reset();
    virtual void setContext(ExecutionContext* __context);
};

class UnaryTupleOperator : public ITupleOperator {
protected:
    MappedTupleIn in;

    UnaryTupleOperator(OPINFO_T _opinfo, unsigned _size, const MappedTupleIn & _in)
        : ITupleOperator(_opinfo, _size), in(_in) {};
public:
    virtual void reset();
    virtual void setContext(ExecutionContext* __context);
};

class ItemOperator : public IValueOperator {
protected:
    IValueOperator * in;

    ItemOperator(OPINFO_T _opinfo, IValueOperator * _in)
      : IValueOperator(_opinfo), in(_in) {};
public:
    virtual void reset();
    virtual void setContext(ExecutionContext* __context);
};

}

#endif /* SEQUENCE_MODEL_H */
