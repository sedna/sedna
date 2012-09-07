#include "DynamicContext.h"

#include "tr/opt/evaluation/VariableMap.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algebra/GraphOperations.h"
#include "tr/opt/algebra/MapOperations.h"
#include "tr/opt/algebra/FunctionOperations.h"
#include "tr/opt/algebra/ElementaryOperations.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/models/SCElementProducer.h"

#include <stack>
#include <queue>

using namespace phop;
using namespace opt;
using namespace rqp;
using namespace executor;

/*
class ConstructorImplementation : public IExecuteProc
{
public:
    DynamicContext * context;
    rqp::Construct * op;

    explicit ConstructorImplementation(DynamicContext * _context, Construct * _op) : context(_context), op(_op) {};
    virtual void execute(VirtualSequence* executor);
};

void ConstructorImplementation::execute(VirtualSequence* result)
{
    xsd::QName nameValue;
    VariableModel * varmodel = context->variables;

    if (op->getType() == element || op->getType() == attribute || op->getType() == pr_ins)
    {
        VirtualSequence nameEvaluation;

        VirtualSequence * saveStack = optimizer->swapStack(&nameEvaluation);
        optimizer->pexecutor()->push(context, op->getName());
        optimizer->swapStack(saveStack);
        
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

    if (op->getType() == element) {
        IElementProducer * parent = context->constructorContext();
        VarCacheInfo * varInfo = varmodel->getProducer(op->contentId);
        context->setConstructorContext(parent->addElement(nameValue));
        VariableProducer * producer;

        if (varInfo == NULL) {
            producer = new VariableProducer(op->contentId);
            varmodel->bind(producer);
        } else {
            producer = varInfo->producer;
            producer->resetResult();
        };

        VirtualSequence * saveStack = optimizer->swapStack(producer->valueSequence);
        optimizer->pexecutor()->push(context, op->getList());
        optimizer->swapStack(saveStack);

        VarIterator contentIterator = varmodel->getIterator(op->contentId);

        while (!contentIterator.next().is_eos()) {
            context->constructorContext()->addValue(contentIterator.get(), false);
        };

        result->push(Result(parent->close()));
        context->setConstructorContext(parent);
    } else {
        U_ASSERT(false);
    };
}

class IterateVar : public IExecuteProc
{
public:
    VarIterator varIterator;

    explicit IterateVar(const VarIterator & _varIterator)
      : varIterator(_varIterator) {};
    
    virtual void execute(VirtualSequence* executor);
};

void IterateVar::execute(VirtualSequence* executor)
{
    varIterator.next();

    if (!varIterator.get().is_eos()) {
        VirtualSequence* saveStack = optimizer->swapStack(executor);

        executor->push(Result(new IterateVar(*this)));
        executor->push(Result(varIterator.get()));

        optimizer->swapStack(saveStack);
    }
}

void PlanExecutor::push(RPBase* op)
{
    VirtualSequence * executionStack = optimizer->currentStack;
    currentContext = context;
    
    switch (op->info()->clsid) {
      CASE_TYPE_CAST(Construct, typed_op, op)
        {
            executionStack->push(Result(new ConstructorImplementation(context, typed_op)));
        }
        break;
      CASE_TYPE_CAST(MapGraph, typed_op, op)
        {
            executionStack->push(Result(typed_op->getExecutor()));
        }
        break;
      CASE_TYPE_CAST(Const, typed_op, op)
        {
            MemoryTupleSequencePtr values = typed_op->getSequence();

            for (MemoryTupleSequence::const_iterator it = values->begin(); it != values->end(); ++it) {
                executionStack->push(Result(*it));
            };
        }
        break;
      CASE_TYPE_CAST(VarIn, typed_op, op)
        {
            executionStack->push(Result(
              new IterateVar(
                context->variables->getIterator(typed_op->tuple()))));
        }
        break;
      case SequenceConcat::opid :
      case MapConcat::opid :
        {
          NestedOperation * nop = static_cast<NestedOperation *>(op);
        }
        break;
      default:
        {
            U_ASSERT(false);
        } break;
    }
}
*/

DynamicContext::DynamicContext()
  : variables(NULL), stack(NULL), updates(NULL)
{
    variables = new VariableModel();
    stack = new VirtualSequence();
    stack->context = this;

    /* TODO: should be NULL and dynamically created by Update operation */
    updates = new UpdateSequence();
}

DynamicContext::~DynamicContext()
{
    delete updates;
    delete stack;
    delete variables;
}
