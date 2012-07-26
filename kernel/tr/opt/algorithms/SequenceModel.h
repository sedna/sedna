/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEQUENCE_MODEL_H
#define SEQUENCE_MODEL_H

#include "tr/models/XmlConstructor.h"
#include "tr/models/rtti.h"

#include "tr/opt/OptTypes.h"

#include "tr/executor/base/tuple.h"

#include <deque>
#include <stack>

namespace phop {

typedef std::vector<IOperator *> Operators;
typedef std::map<IOperator *, Operators::size_type> OperatorMap;
typedef std::map<opt::TupleId, unsigned> TupleIdMap;

struct OperationFlags
{
    uint64_t changed_flags;
};

class GraphExecutionBlock {
    static std::stack<GraphExecutionBlock * > blockBuildingStack;
public:
    static GraphExecutionBlock * current()
    {
        return blockBuildingStack.top();
    };

    static GraphExecutionBlock * push(GraphExecutionBlock * executionBlock)
    {
        GraphExecutionBlock * result = blockBuildingStack.empty() ? NULL : blockBuildingStack.top();
        blockBuildingStack.push(executionBlock);
        return result;
    };

    static GraphExecutionBlock * pop()
    {
        GraphExecutionBlock * result = blockBuildingStack.top();
        blockBuildingStack.pop();
        return result;
    };
private:
    friend class IOperator;
public:
    OperationFlags flags;

    executor::DynamicContext * context;
    void setContext(executor::DynamicContext * __context) { context = __context; };

    Operators body;
    OperatorMap operatorMap;

    std::stack<opt::POProt *> sourceStack;

// TODO : this is not actual result map!!!    
   TupleIdMap resultMap;

    GraphExecutionBlock() {};
    ~GraphExecutionBlock() {};

    GraphExecutionBlock * copy();

    unsigned outputTupleId;

    void prepare(const opt::DataGraphIndex * dgi);

    inline
    ITupleOperator * top()
    {
        return (ITupleOperator *)(body.back());
    };
};

class IOperator : public ObjectBase, public IXMLSerializable {
    RTTI_DECL(sequence_operator_IOperator, ObjectBase)
protected:    
    virtual XmlConstructor & __toXML(XmlConstructor &) const = 0;
protected:
    GraphExecutionBlock * block;

    IOperator(clsinfo_t _opinfo);

    virtual void do_next() = 0;
public:
    virtual ~IOperator() {};
    virtual void reset() = 0;

    OperationFlags & flags() { return block->flags; };
    virtual XmlConstructor & toXML(XmlConstructor &) const;
};

class IValueOperator : public IOperator {
    RTTI_DECL(sequence_operator_IValueOperator, IOperator)

    std::deque<tuple_cell> valueCache;
protected:
    void seteos() { valueCache.push_back(tuple_cell()); };
    void push(const tuple_cell & tc) { valueCache.push_back(tc); };

    IValueOperator(clsinfo_t _opinfo) : IOperator(_opinfo) {};
public:
    virtual void reset()
      { valueCache.clear(); };

    inline bool next()
    {
        if (!valueCache.empty()) {
            if (get().is_eos()) {
                return false;
            };

            valueCache.pop_front();
        }

        if (valueCache.empty()) {
            do_next();
        };

        return true;
    };
    
    const tuple_cell& get() const { return valueCache.front(); };
};

class ITupleOperator : public IOperator {
    RTTI_DECL(sequence_operator_ITupleOperator, IOperator)

    tuple _value;
protected:
    void seteos() { _value.set_eos(); };
    tuple & value() { return _value; };

    virtual void do_next() = 0;
    
    ITupleOperator(clsinfo_t _opinfo, unsigned _size)
      : IOperator(_opinfo), _value(_size) {};
public:
    virtual void reset()
      { _value.eos = false; };

    inline bool next() {
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

    static void tupleAssignTo(tuple & result, const tuple & from, const TupleMap & tmap) {
        if (tmap.size() > 0) {
            for (TupleMap::const_iterator it = tmap.begin(); it != tmap.end(); ++it) {
                result.cells[it->second] = from.cells[it->first];
            };
        }
    };

    void assignTo(tuple & result, const TupleMap & tmap) const {
        tupleAssignTo(result, op->get(), tmap);
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

    void tupleAssignTo(tuple & result, const tuple & from) const {
        TupleIn::tupleAssignTo(result, from, tmap);
    }

    void assignTo(tuple & result) const { TupleIn::assignTo(result, tmap); }
};

class ReduceToItemOperator : public IValueOperator {
    RTTI_DECL(sequence_operator_ReduceToItemOperator, IValueOperator)
private:
    Operators::size_type inIdx;
protected:
    TupleIn in;
    bool nested;

    virtual void do_next();
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    ReduceToItemOperator(const TupleIn & op, bool nested);

    virtual void reset();
    virtual XmlConstructor & toXML(XmlConstructor &) const;
};

class BinaryTupleOperator : public ITupleOperator {
    RTTI_DECL(sequence_operator_BinaryTupleOperator, ITupleOperator)
private:
    Operators::size_type leftIdx, rightIdx;
protected:
    MappedTupleIn left, right;

    BinaryTupleOperator(clsinfo_t _opinfo, unsigned _size, const MappedTupleIn & _left, const MappedTupleIn & _right);
    
    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    const MappedTupleIn & getLeft() const { return left; }
    const MappedTupleIn & getRight() const { return right; }
  
    virtual void reset();
};

class UnaryTupleOperator : public ITupleOperator {
    RTTI_DECL(sequence_operator_UnaryTupleOperator, ITupleOperator)
private:
    Operators::size_type inIdx;
protected:
    MappedTupleIn in;

    UnaryTupleOperator(clsinfo_t _opinfo, unsigned _size, const MappedTupleIn & _in);

    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    const MappedTupleIn & getIn() const { return in; }
    
    virtual void reset();
};

class ItemOperator : public IValueOperator {
    RTTI_DECL(sequence_operator_ItemOperator, IValueOperator)
private:
    Operators::size_type inIdx;
protected:
    IValueOperator * in;
    
    ItemOperator(clsinfo_t _opinfo, IValueOperator * _in);

    virtual XmlConstructor& __toXML(XmlConstructor& ) const;
public:
    virtual void reset();
};

}

#endif /* SEQUENCE_MODEL_H */
