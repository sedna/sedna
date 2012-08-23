#ifndef _OPTIMIZING_EXECUTOR_H_
#define _OPTIMIZING_EXECUTOR_H_

#include "common/u/uthread.h"
#include "common/u/utime.h"

#include "tr/models/SednaModule.h"
#include "tr/models/MemoryPool.h"
#include "tr/models/PointerArray.h"
#include "tr/opt/OptForewards.h"

#include "tr/opt/types/ResultQueue.h"

#include "tr/tr_base.h"

struct GlobalSerializationOptions;

class Serializer;
class client_core;

namespace sedna {
  class XQueryDriver;
}

class BlockingItemQueue;
class SednaException;

namespace opt {

struct execution_params_t
{
    unsigned operationTimeout;
    unsigned executionTimeout;

    unsigned operationStackSize;
    unsigned executionStackSize;
};

const execution_params_t defaultSettings =
{
    30*60,
    60*60,
    1024,
    16*1024*1024
};

class IStatement
{
protected:
    bool update;
    bool finished;
public:
    virtual ~IStatement() {};

    virtual void prepare(QueryType queryType, const char* query_str) = 0;
    virtual void execute() = 0;
    virtual void next() = 0;

    bool isUpdate() const { return update; };
    bool isFinished() const { return finished; };
};

class OptimizedStatement : public IStatement
{
protected:
    client_core * client;
    sedna::XQueryDriver * driver;
    rqp::RPBase * plan;
    executor::DynamicContext * executionContext;
    Serializer * serialier;
    GlobalSerializationOptions * serializationOptions;
public:
    execution_params_t params;
    ex_time_t time;

    OptimizedStatement(client_core * _client);
    virtual ~OptimizedStatement();

    virtual void prepare(QueryType queryType, const char* query_str);
    virtual void execute();
    virtual void next();
};

class ExplainStatement : public OptimizedStatement
{
public:
    ExplainStatement(client_core* _client) : OptimizedStatement(_client) {};

    virtual void execute();
    virtual void next();
};

class OptimizingExecutor : SednaModule
{
private:
    opt::GraphCompiler * _gcmpler;
    VariableUsageGraph * _dgm;
    rqp::PlanContext * _planContext;
    CostModel * _costModel;
public:
    MemoryPool planGenerationPool;
    MemoryPool costModelPool;

    OptimizingExecutor()
      : _gcmpler(NULL), _dgm(NULL), _planContext(NULL), _costModel(NULL),
        planGenerationPool(MEMORY_BLOCK_SIZE),
        costModelPool(MEMORY_BLOCK_SIZE)
          {};

    virtual void onSessionBegin();
    virtual void onSessionEnd();

    virtual void onTransactionBegin();
    virtual void onTransactionEnd();

    executor::VirtualSequence * currentStack;

    executor::VirtualSequence * swapStack(executor::VirtualSequence * stack)
    {
        executor::VirtualSequence * old = currentStack;
        currentStack = stack;
        return old;
    };

    opt::GraphCompiler * gcmpler() const { return _gcmpler; };
    rqp::PlanContext * planContext() const { return _planContext; };
    CostModel * costModel() const { return _costModel; };
};

};

extern opt::OptimizingExecutor * optimizer;

#endif /* _OPTIMIZING_EXECUTOR_H_ */
