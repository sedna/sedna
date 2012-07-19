#include "ExecutionContext.h"

#include "tr/opt/algorithms/VariableMap.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algebra/GraphOperations.h"
#include "tr/opt/algebra/MapOperations.h"
#include "tr/opt/algebra/FunctionOperations.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/SequenceModel.h"

#include <stack>
#include <queue>

using namespace phop;
using namespace opt;
using namespace rqp;
using namespace executor;

static
void constructorExecute(void * object, executor::ResultSequence * result)
{
    rqp::Construct * ths = (rqp::Construct *) object;
    xsd::QName nameValue;
    VariableModel * varmodel = result->context->variables; 

    if (ths->getType() == element || ths->getType() == attribute || ths->getType() == pr_ins)
    {
        ResultSequence nameEvaluation(result->context);
        nameEvaluation.push(
          optimizer->executor()->getOperationResult(ths->getName(), result->context));
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

    VarIterator contentIterator = varmodel->getIterator(ths->contentId);

    if (ths->getType() == element) {
        IElementProducer * parent = result->context->constructorContext;
        DynamicContext * newContext;

        VarCacheInfo * varInfo = varmodel->getProducer(ths->contentId);

        if (varInfo == NULL) {
            newContext = optimizer->executor()->newContext(*result->context);
            newContext->constructorContext = parent->addElement(nameValue);

            varmodel->bind(ths->contentId,
                new VariableProducer(
                    optimizer->executor()->getOperationResult(ths->getList(), newContext),
                    ths->contentId, newContext));
        } else {
            newContext = varInfo->producer->valueSequence->context;
            newContext->constructorContext = parent->addElement(nameValue);
            varInfo->producer->resetResult(
                optimizer->executor()->getOperationResult(ths->getList(), newContext));
        };

        while (!contentIterator.next().is_eos()) {
            newContext->constructorContext->addValue(contentIterator.get(), false);
        };

        result->push(Result(parent->close()));
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


static
void mapGraphNext(void * object, executor::ResultSequence * result)
{
    MapGraph * mapGraph = static_cast<MapGraph *>(object);
    VariableProducer * producer = mapGraph->tag;
    uint64_t saveRestrickMask = producer->restrictMask;

    if (saveRestrickMask == 0) {
        saveRestrickMask = getRestrictMask(mapGraph->tupleMask);
    };

    producer->restrictMask = 0;
    producer->next();
    producer->restrictMask = saveRestrickMask;

    result->push(Result(NextResultProc(mapGraphNext, mapGraph)));
    result->push(optimizer->executor()->getOperationResult(mapGraph->getList(), result->context));
};

Result PlanExecutor::getOperationResult(RPBase* op, DynamicContext* context)
{
    switch (op->info()->opType) {
      case Construct::opid :
        {
            return Result(NextResultProc(constructorExecute, op));
        }
        break;
      case MapGraph::opid :
        {
            MapGraph * m = static_cast<MapGraph *>(op);
            // TODO: optimize graph execution. There is no need in most cases to rebuild the graph
            GraphExecutionBlock * geb = gc.compile(m->graph());
            m->tag = context->variables->bindGraph(geb, &(m->graph()));
            return Result(NextResultProc(mapGraphNext, m));
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
    //
}

