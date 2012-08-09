#ifndef _VARIABLE_MAP_H_
#define _VARIABLE_MAP_H_

#include "tr/executor/base/sequence.h"
#include "tr/opt/evaluation/VirtualSequence.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace executor {

struct VarCacheInfo
{
    opt::TupleId tid;
    sequence * seq;
    VariableProducer * producer;
    unsigned inTuple;

    opt::TupleStatistics * statistics;
    opt::tuple_info_t properties;

    VarCacheInfo()
      : tid(opt::invalidTupleId), statistics(NULL)
    {
        // TODO : add variable no cache optimization
        seq = new sequence(1, 1024);
    }

    ~VarCacheInfo() { if (seq != NULL) { delete seq; } };
};

struct VariableProducer
{
    executor::VirtualSequence * valueSequence;
    phop::GraphExecutionBlock * graphSequence;
    VarCacheInfo * variables;
    size_t varCount;
    int generation; /* uninitialized, just id */
    uint64_t restrictMask;

    VariableProducer(opt::TupleId varId)
      : varCount(1)
    {
        valueSequence = new executor::VirtualSequence();

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
            variables[i].tid = prototype->out[i]->varTupleId;
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

        for (size_t i = 0; i < varCount; ++i) {
            variables[i].seq->clear();
        };
    };

    inline
    void reset()
    {
        generation++;

        if (valueSequence != NULL) {
            valueSequence->clear();
        } else {
            graphSequence->top()->reset();
        };

        clear();
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

    inline
    bool next()
    {
        if (valueSequence != NULL) {
            if (restrictMask > 0) {
                return false;
            };

            tuple_cell x = valueSequence->next();

            if (x.is_eos()) {
                return false;
            };

            // TODO: get rid of tuple here!
            variables->seq->add(tuple(x));
        } else {
            if ((graphSequence->flags.changed_flags & restrictMask) > 0) {
                return false;
            };

            if (!graphSequence->next()) {
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

class VarIterator
{
    int pos;
    VarCacheInfo * varInfo;
    tuple_cell value;
    int generation;
public:
    const VarCacheInfo * info() const { return varInfo; }

    explicit VarIterator(VarCacheInfo * _varInfo)
      : pos(-1), varInfo(_varInfo), value(EMPTY_TUPLE_CELL)
    {
        generation = varInfo->producer->generation;
    };

    inline
    tuple_cell & get()
    {
        return value;
    };

    inline
    void reset()
    {
        generation = varInfo->producer->generation;
        pos = -1;
        value = EMPTY_TUPLE_CELL;
    };

    inline
    tuple_cell & next()
    {
        if (generation != varInfo->producer->generation) {
            reset();
        };

        if ((pos > -1) && value.is_eos()) {
            return value;
        };

        ++pos;

        while (varInfo->seq->size() == pos)
        {
            if (!varInfo->producer->next())
            {
                value.set_eos();
                return get();
            };
        }

        tuple x(1);
        varInfo->seq->get(x, pos);
        value = x[0];

        return get();
    };
};

typedef std::map<opt::TupleId, VarCacheInfo *> VariableMap;
typedef std::map<phop::GraphExecutionBlock *, VariableProducer *> VariableProducerMap;

class VariableModel
{
    VariableMap variableMap;
    VariableProducerMap graphMap;
    object_vector<VariableProducer> producers;
public:
    VariableProducer * bindGraph(phop::GraphExecutionBlock * graph, const opt::DataGraphIndex * prototype)
    {
        // TODO : make debug check for all graph variables to be bound to valid producer

        VariableProducer * producer;

        /* Create or find producer map for a graph.
         * Kind of optimization.
         */

        if (graphMap.find(graph) == graphMap.end())
        {
            producer = new VariableProducer(graph, prototype);
            graphMap.insert(VariableProducerMap::value_type(graph, producer));
            producers.push_back(producer);
        } else {
            producer = graphMap.at(graph);
        };

        /* Rebind all variables
         * TODO : do not rebind variables if they are bound already
         */

        for (size_t i = 0; i < producer->varCount; ++i)
        {
            variableMap[producer->variables[i].tid] = producer->variables + i;
        };

        return producer;
    };

    /* Get variable descriptor for variable producer */
    VarCacheInfo * getProducer(opt::TupleId var)
    { 
        VariableMap::const_iterator it = variableMap.find(var);
        return (it == variableMap.end()) ? NULL : it->second;
    };

    void bind(VariableProducer * producer)
    {
        producers.push_back(producer);

        for (size_t i = 0; i < producer->varCount; ++i)
        {
            variableMap[producer->variables[i].tid] = producer->variables + i;
        };
    };

    /* Generate Iterator for a variable */
    VarIterator getIterator(opt::TupleId var)
    {
        VariableMap::const_iterator it = variableMap.find(var);

        if (it == variableMap.end())
        {
            U_ASSERT(false);
        //throw USER_EXCEPTION
        };

        return VarIterator(it->second);
    }
};

};

#endif /* _VARIABLE_MAP_H_ */
