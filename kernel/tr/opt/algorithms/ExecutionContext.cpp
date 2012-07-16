#include "ExecutionContext.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algebra/GraphOperations.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/SequenceModel.h"

#include <stack>
#include <queue>

using namespace phop;

template <typename T>
class object_vector : public std::vector<T *>
{
typedef std::vector<T *> base_t;
public:
    ~object_vector() {
        for (base_t::const_iterator it = base_t::begin(); it != base_t::end(); ++it) {
            delete *it;
        };
    };
};

struct Block
{
    virtual void execute();
};

struct Result
{
    tuple_cell value;
    
};

struct Context
{
    ExecutionContext context;
    VariableMap varMap;
    object_vector<Producer> producerList;
    std::stack<Block *> executionStack;
};

struct GroupByIterateTuple : public Block
{
    Block * next;
    ExecutionBlock * plan = NULL;
    unsigned tid;

    virtual void execute(Context * context)
    {
        plan->top()->flags().clear_changed();
        
        do {
            plan->top()->next();
        } while (!plan->top()->flags().changed(tid));

        context->executionStack.push(next);
    };
};

struct VarIn : public Block
{
    Block * next;
    ExecutionBlock * plan = NULL;
    unsigned tid;

    virtual void execute(Context * context)
    {
        plan->top()->flags().clear_changed();

        do {
            plan->top()->next();
        } while (!plan->top()->flags().changed(tid));

        context->executionStack.push(next);
    };
};

void phop::PhysicalExecutor::execute(rqp::RPBase* base)
{
    scoped_ptr<ExecutionContext> context(new ExecutionContext);
    scoped_ptr<VariableMap> varMap(new VariableMap);
    object_vector<Producer> producerList;
    ProducerMap producerMap;

    do {
        GroupByIterateTuple * it;
        it->next();
    } while (true);
    
    switch (base->info()->opType) {
      case rqp::MapGraph :
      {
          rqp::MapGraph * op = static_cast<rqp::MapGraph *>(base);
          DataGraphIndex & dgw = op->graph();
          TupleId anyTid = *op->tupleMask.begin();

          /* Here we try to find any variable, no matter which one
           */

          Producer * varProducer = NULL;
          
          if (producerMap.find(anyTid) != producerMap.end())
          {
              varProducer = producerMap.at(anyTid);
              varProducer->reset();
          } else {
              optimizer->dgm()->compile(dgw.dg);
              varProducer = new Producer(top);

              for (DataNodeList::const_iterator it = dgw.out.begin(); it != dgw.out.end(); ++it)
              {
                  DataNode * dn = *it;
                  varProducer->map(dn->varTupleId, dn->absoluteIndex);
                  varMap->bind(dn->varTupleId, varProducer);
              };
          };

          GroupByIterateTuple * iterator;
      }; break;
      default :
        U_ASSERT(false);
    };
}
