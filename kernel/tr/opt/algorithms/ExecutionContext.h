#ifndef _EXECUTION_CONTEXT_H_
#define _EXECUTION_CONTEXT_H_

#include "tr/opt/OptTypes.h"
#include "tr/executor/base/sequence.h"

namespace rqp {
class RPBase;
}

class CollationHandler;

using namespace opt;

namespace phop {

class StaticContext {
};

struct Producer
{
    union {
        ITupleOperator * top;
    };
  
    void map(TupleId tid, unsigned varid);
    void reset();
    void reset(ITupleOperator * op);
  
    bool next(TupleId tid)
    {
        return false;
    };
};

class VarIterator
{
    TupleId tid;
    sequence * seq;
    int pos;
    Producer * producer;
    tuple_cell value;
public:
    VarIterator(TupleId _tid, sequence * _seq, int _pos, Producer * _producer)
      : tid(_tid), seq(_seq), pos(_pos), producer(_producer), value(tuple_cell::eos())
    {
    };
    
    tuple_cell get()
    {
        return value;
    };

    tuple_cell next()
    {
        if (value.eos()) {
            return value;
        };

        while (seq->size() == pos)
        {
            if (!producer->next())
            {
                value.set_eos();
                break;
            };
        }
        
        tuple x(1);
        seq->get(x, pos);
        value = x[0];
        ++pos;

        return get();
    };
};

class VariableMap {
    
public:
    VarIterator & get(opt::TupleId tid);
    void bind(opt::TupleId tid, Producer * producer);
};

class DynamicContext {
public:
//    Sequence * ;
};

class ExecutionContext {
public:
    CollationHandler * collation;
};

typedef std::map<opt::TupleId, Producer *> ProducerMap;

class PhysicalExecutor
{
public:
    void execute(rqp::RPBase * base);
};

}

#endif /* _EXECUTION_CONTEXT_H_ */
