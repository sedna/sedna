#include "ExecutionContext.h"

#include "tr/opt/algorithms/VariableMap.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algebra/GraphOperations.h"
#include "tr/opt/algebra/MapOperations.h"
#include "tr/opt/algebra/FunctionOperations.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/SequenceModel.h"
#include "tr/models/SCElementProducer.h"

#include <stack>
#include <queue>

using namespace phop;
using namespace opt;
using namespace rqp;
using namespace executor;

class ConstructorImplementation : public IExecuteProc
{
public:
    DynamicContext * context;
    rqp::Construct * op;

    explicit ConstructorImplementation(DynamicContext * _context, Construct * _op) : context(_context), op(_op) {};
    virtual void execute(ExecutionStack* executor);
};

class GroupNextImplementation : public IExecuteProc
{
public:
    DynamicContext * context;
    VariableProducer * producer;
    rqp::RPBase * nextOp;
    uint64_t tupleMask;
    
    explicit GroupNextImplementation(DynamicContext * _context, VariableProducer * _producer, RPBase * _nextOp, uint64_t _tupleMask)
      : context(_context), producer(_producer), nextOp(_nextOp), tupleMask(_tupleMask) {};

    virtual void execute(ExecutionStack* executor);
};

void ConstructorImplementation::execute(ExecutionStack* result)
{
    xsd::QName nameValue;
    VariableModel * varmodel = context->variables;

    if (op->getType() == element || op->getType() == attribute || op->getType() == pr_ins)
    {
        ExecutionStack nameEvaluation;
        nameEvaluation.push(
          optimizer->executor()->getOperationResult(op->getName(), context));
        tuple_cell qname_tc = nameEvaluation.next();

        if (qname_tc.get_atomic_type() != xs_QName)
        {
            U_ASSERT(false);
            // TODO: USER_Exception
        };

        nameValue = qname_tc.get_xs_qname();

        if (!nameEvaluation.next().is_eos())
        {
            U_ASSERT(false);
            // TODO: USER_Exception
        };
    }

    VarIterator contentIterator = varmodel->getIterator(op->contentId);

    if (op->getType() == element) {
        IElementProducer * parent = context->constructorContext;
        VarCacheInfo * varInfo = varmodel->getProducer(op->contentId);
        context->constructorContext = parent->addElement(nameValue);
        
        if (varInfo == NULL) {
            varmodel->bind(op->contentId,
                new VariableProducer(
                    optimizer->executor()->getOperationResult(op->getList(), context),
                      op->contentId));
        } else {
            varInfo->producer->resetResult(
                optimizer->executor()->getOperationResult(op->getList(), context));
        };

        while (!contentIterator.next().is_eos()) {
            context->constructorContext->addValue(contentIterator.get(), false);
        };

        result->push(Result(parent->close()));
        context->constructorContext = parent;
    } else {
        U_ASSERT(false);
    };
}

uint64_t getRestrictMask(const TupleScheme & tscheme)
{
    uint64_t result;

    for (TupleScheme::const_iterator it = tscheme.begin(); it != tscheme.end(); ++it)
    {
        result |= (1ULL << *it);
    };

    return result;
};

void GroupNextImplementation::execute(ExecutionStack* result)
{
    uint64_t saveRestrickMask = producer->restrictMask;

    if (saveRestrickMask == 0) {
        saveRestrickMask = tupleMask;
    };

    producer->restrictMask = 0;
    producer->next();
    producer->restrictMask = saveRestrickMask;

    result->push(Result(new GroupNextImplementation(*this)));
    result->push(optimizer->executor()->getOperationResult(nextOp, context));
};

Result PlanExecutor::getOperationResult(RPBase* op, DynamicContext* context)
{
    switch (op->info()->opType) {
      case Construct::opid :
        {
            return Result(new ConstructorImplementation(context, static_cast<Construct *>(op)));
        }
        break;
      case MapGraph::opid :
        {
            MapGraph * mapGraph = static_cast<MapGraph *>(op);
            uint64_t tupleMask = getRestrictMask(mapGraph->tupleMask);
            // TODO: optimize graph execution. There is no need in most cases to rebuild the graph
            GraphExecutionBlock * geb = gc.compile(mapGraph->graph());
            VariableProducer * producer = context->variables->bindGraph(geb, &(mapGraph->graph()));
            return Result(new GroupNextImplementation(context, producer, mapGraph->getList(), tupleMask));
        }
        break;
/*
      case SequenceConcat::opid :
      case MapConcat::opid :
        {
          NestedOperation * nop = static_cast<NestedOperation *>(op);
        }
        break;
*/
      default:
        {
            U_ASSERT(false);
            return Result(EMPTY_TUPLE_CELL);
        } break;
    }
}

PlanExecutor::PlanExecutor()
{
    executionStack = new ExecutionStack();
//    baseContext.constructorContext = SCElementProducer::getVirtualRoot(XNULL);
    baseContext.variables = new VariableModel;
}

PlanExecutor::~PlanExecutor()
{
    delete executionStack;
    delete baseContext.variables;
//    delete baseContext.constructorContext;
}

void PlanExecutor::execute(RPBase* op)
{
    executionStack->push(getOperationResult(op, &baseContext));
}
