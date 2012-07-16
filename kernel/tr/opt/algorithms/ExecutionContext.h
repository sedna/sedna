#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/executor/base/sequence.h"

namespace rqp {
class RPBase;
}

class CollationHandler;

namespace phop {

class ExecutionBlock;

class StaticContext {
};

struct VariableProducer
{
    union {
        ExecutionBlock * top;
        rqp::RPBase * op;
    };

    uint64_t boundVarMask;

    bool next()
    {
        return false;
    };
};

struct VarCacheInfo
{
    opt::TupleId tid;
    sequence * seq;
    VariableProducer * producer;
};

class VarIterator
{
    int pos;
    VarCacheInfo * varInfo;
    tuple_cell value;
public:
    VarIterator(VarCacheInfo * _varInfo)
      : pos(-1), varInfo(_varInfo), value(EMPTY_TUPLE_CELL)
    {
    };
    
    tuple_cell & get()
    {
        return value;
    };

    tuple_cell & next()
    {
        if ((pos > -1) && value.is_eos()) {
            return value;
        };

        ++pos;
        
        while (varInfo->seq->size() == pos)
        {
            if (!varInfo->producer->next())
            {
                value.set_eos();
                break;
            };
        }
        
        tuple x(1);
        varInfo->seq->get(x, pos);
        value = x[0];

        return get();
    };
};

class DynamicContext {
public:
//    Sequence * ;
};

class ExecutionContext {
public:
    CollationHandler * collation;
};

//typedef std::map<opt::TupleId, Producer *> ProducerMap;

struct result_t
{
    tuple_cell value;
    rqp::RPBase * action;
    phop::ExecutionBlock * plan;
    unsigned tupleIndex;

    result_t(const tuple_cell & _tc) : value(_tc) {};
    result_t(rqp::RPBase * act) : action(act) {};
    result_t(phop::ExecutionBlock * _block, unsigned _tupleIndex) : action(NULL), plan(_block), tupleIndex(_tupleIndex) {};
};

class ConstructorContext
{
public:
    IElementProducer * producer();
    IElementProducer * push(IElementProducer *);
    IElementProducer * pop();
};

typedef std::list<result_t> ResultStack;

class PlanExecutor
{
public:
    ResultStack result;
    ResultStack::iterator resultIterator;
  
    VariableProducer * getProducer(opt::TupleId var);
    VarIterator getVarIterator(opt::TupleId var);
    ConstructorContext * getConstructorContext();

    void bind(opt::TupleId var, VariableProducer * producer);
    void bind(const opt::TupleScheme & vars, VariableProducer * producer, const opt::TupleScheme & dependsOn);
    void unbind(opt::TupleScheme vars);
};

}

#endif /* _EXECUTION_CONTEXT_H_ */
