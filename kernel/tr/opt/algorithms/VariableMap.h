#ifndef _VARIABLE_MAP_H_
#define _VARIABLE_MAP_H_

#include "tr/executor/base/sequence.h"
#include "tr/opt/algorithms/ExecutionStack.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace executor {

struct VariableProducer;
  
struct VarCacheInfo
{
    opt::TupleId tid;
    sequence * seq;
    VariableProducer * producer;
    unsigned inTuple;

    VarCacheInfo()
      : tid(opt::invalidTupleId)
    {
        // TODO : add variable no cache optimization
        seq = new sequence();
    }

    ~VarCacheInfo() { if (seq != NULL) { delete seq; } };
};

struct VariableProducer
{
    executor::ResultSequence * valueSequence;
    phop::GraphExecutionBlock * graphSequence;
    VarCacheInfo * variables;
    size_t varCount;
    int generation; /* uninitialized, just id */
    uint64_t restrictMask;

    VariableProducer(const executor::Result & result, opt::TupleId varId)
      : varCount(1)
    {
        valueSequence = new executor::ResultSequence();
        valueSequence->push(result);

        variables = new VarCacheInfo[1];
        variables[0].producer = this;
        variables[0].tid = varId;
    };

    VariableProducer(phop::GraphExecutionBlock * graph, const opt::DataGraphIndex * prototype)
      : valueSequence(NULL), graphSequence(graph), varCount(0), restrictMask(0)
    {
        varCount = prototype->out.size();
        variables = new VarCacheInfo[varCount];

        for (size_t i = 0; i < varCount; ++i)
        {
            variables[i].producer = this;
            variables[i].tid = prototype->out.at(i)->varTupleId;
            variables[i].inTuple = graph->resultMap.at(variables[i].tid);
        };
    };

    ~VariableProducer()
    {
        if (valueSequence != NULL) {
            delete valueSequence;
        } else {
            delete graphSequence;
        };

        delete[] variables;
    };

    inline
    void clear()
    {
        generation++;

        if (valueSequence != NULL) {
            valueSequence->clear();
        } else {
            graphSequence->top()->reset();
        };

        for (size_t i = 0; i < varCount; ++i) {
            variables[i].seq->clear();
        };
    };

    void resetGraph(phop::GraphExecutionBlock * graph, const opt::DataGraphIndex * prototype)
    {
        clear();
        U_ASSERT(valueSequence == NULL);

        if (graph != graphSequence) {
            U_ASSERT(varCount == prototype->out.size());
            
            graphSequence = graph;

            for (size_t i = 0; i < varCount; ++i)
            {
                variables[i].tid = prototype->out.at(i)->varTupleId;
                variables[i].inTuple = graph->resultMap.at(variables[i].tid);
            };
        };
    }

    void resetResult(const executor::Result & result)
    {
        clear();
        valueSequence->push(result);
    }

    inline
    bool next()
    {
        if (valueSequence != NULL) {
            tuple_cell x = valueSequence->next();

            if (x.is_eos()) {
                return false;
            };
            
            // TODO: get rid of tuple here!
            variables->seq->add(tuple(x));
        } else {
            graphSequence->top()->next();

            if (graphSequence->top()->get().is_eos()) {
                return false;
            };

            if ((graphSequence->flags.changed_flags & restrictMask) > 0)
            {
                return false;
            };
            
            for (size_t i = 0; i < varCount; ++i) {
                unsigned tid = variables[i].inTuple;

                if ((graphSequence->flags.changed_flags & (1ULL << tid)) > 0) {
                    // TODO: get rid of tuple here!
                    variables[i].seq->add(tuple(graphSequence->top()->get().cells[tid]));
                }
            };
        };

        return true;
    };
};

typedef std::map<opt::TupleId, VarCacheInfo *> VariableMap;

class VarIterator
{
    int pos;
    VarCacheInfo * varInfo;
    tuple_cell value;
// TODO : add generation debug    
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

class VariableModel
{
    VariableMap variableMap;
    object_vector<VariableProducer> producers;
public:
    void bind(opt::TupleId var, VariableProducer * producer)
    {
    };
  
    void bind(const opt::TupleScheme & vars, VariableProducer * producer);
};

};

#endif /* _VARIABLE_MAP_H_ */