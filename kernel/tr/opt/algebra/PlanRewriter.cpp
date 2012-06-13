#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algebra/DataGraph.h"
#include "tr/opt/algebra/Predicates.h"
#include "tr/opt/functions/Functions.h"

using namespace opt;
using namespace rqp;

struct DataReductionBlock {
    RPEdge in;
    OperationList out;
    OperationList members;

    DataGraph* buildBlock();
};

class DataGraphReduction {
    std::vector<DataReductionBlock *> blocks;
    std::map<RPBase *, DataReductionBlock *> opBlockMap;
    std::stack<DataReductionBlock *> blockStack;

    DataReductionBlock * newBlock()
    {
        DataReductionBlock * result = new DataReductionBlock();
        blocks.push_back(result);
        blockStack.push(result);
        return result;
    }

    void addOp(RPBase * op)
    {
        U_ASSERT(blockStack.top() != NULL);
        blockStack.top()->members.push_back(op);
        opBlockMap[op] = blockStack.top();
    };
    
public:
    void collectBlocks(RPBase * parent, RPBase * op);
    void execute(RPBase * op);
};

typedef std::map<RPBase *, DataNode *> ResultMap;
typedef std::multimap<opt::TupleId, opt::TupleId> ExternalMap;

struct Generator {
    ResultMap map;
    DataGraph * dg;

    DataNode * get(RPBase * op) {
        DataNode * x = NULL;

        if (map.find(op) == map.end()) {
            x = dg->owner->createNode(dg);
            x->type = DataNode::dnExternal;
            map.insert(ResultMap::value_type(op, x));
        } else {
            x = map.at(op);
        };

        return x;
    };
};

static
std::string varName(rqp::VarIn * op) {
    return std::string(op->getContext()->getVarDef(op->getTuple())->getVarLabel());
};

DataGraph * DataReductionBlock::buildBlock()
{
    DataGraphMaster * dgm = PlanContext::current->dgm();
    Generator context;
    context.dg = dgm->createGraph();
    DataNode * node;

    for (OperationList::const_iterator it = members.begin(); it != members.end(); ++it) {
        RPBase * op = *it;

        switch (op->info()->opType) {
          case rqp::Select::opid : {
            U_ASSERT(dynamic_cast<rqp::Select *>(op) != NULL);
            rqp::Select * sop = static_cast<rqp::Select *>(op);

            node = context.get(op);
            node->type = DataNode::dnAlias;
            node->source = context.get(sop->getList());
            node->varTupleId = sop->getContextTuple();
            
            dgm->variableMap.insert(VariableMap::value_type(node->varTupleId, node));
          } break;
          case rqp::Const::opid :
            U_ASSERT(dynamic_cast<rqp::Const *>(op) != NULL);
            
            node = context.get(op);
            node->type = DataNode::dnConst;
            node->sequence = static_cast<rqp::Const *>(op)->getSequence();
            
            break;
          case rqp::VarIn::opid : {
            U_ASSERT(dynamic_cast<rqp::VarIn *>(op) != NULL);
            rqp::VarIn * vop = static_cast<rqp::VarIn *>(op);
            
            node = context.get(op);
            node->type = DataNode::dnExternal;
            node->varName = varName(vop);
            node->varTupleId = vop->getTuple();

            dgm->variableMap.insert(VariableMap::value_type(node->varTupleId, node));
          } break;
          case rqp::XPathStep::opid : {
            U_ASSERT(dynamic_cast<rqp::XPathStep *>(op) != NULL);
            
            rqp::XPathStep * xop = static_cast<rqp::XPathStep *>(op);
            node = context.get(op);
            node->type = DataNode::dnFreeNode;

            new SPredicate(context.dg, context.get(xop->getList()), node, xop->getStep());
          } break;
          case rqp::ComparisonExpression::opid : {
            U_ASSERT(dynamic_cast<rqp::ComparisonExpression *>(op) != NULL);

            rqp::ComparisonExpression * cop = static_cast<rqp::ComparisonExpression *>(op);
            
            node = context.get(op);
            node->type = DataNode::dnConst;

            node->sequence = new MemoryTupleSequence();
            node->sequence->push_back(tuple_cell::atomic(true));

            new VPredicate(context.dg, context.get(cop->getLeft()), context.get(cop->getRight()), cop->getOp());
          } break;
          case rqp::FunCall::opid : {
            U_ASSERT(dynamic_cast<rqp::FunCall *>(op) != NULL);

            rqp::FunCall * cop = static_cast<rqp::FunCall *>(op);

            node = context.get(op);
            node->type = DataNode::dnFreeNode;

            // TODO: Fix number of parameters
            new FPredicate(context.dg, context.get(cop->getOperations().at(0)), node, NULL);
          } break;
          default:
            U_ASSERT(false);
          break;
        };
    };

    context.get(in.second)->output = true;

    return context.dg;
}


void DataGraphReduction::collectBlocks(RPBase * parent, RPBase * op)
{
    bool blockOwner = false;
    bool blockBuilder = (op->info()->flags & rqp::oBlockBuilder) > 0;

    OperationList children;
    op->getChildren(children);

    if ((op->info()->flags & rqp::oBlockSpecial) > 0) {
        switch (op->info()->opType) {
          case FunCall::opid : {
            blockBuilder = static_cast<FunCall *>(op)->getFunction()->getFlag(phop::fn_in_block);
          }; break;
        };
    }

    if (blockBuilder) {
        blockOwner = blockStack.top() == NULL;

        if (blockOwner) {
            newBlock();
            blockStack.top()->in = RPEdge(parent, op);
        };

        addOp(op);
    } else if (blockStack.top() != NULL) {
        blockStack.top()->out.push_back(op);
        blockStack.push(NULL);
        blockOwner = true;
    };

    for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
        if (*it != NULL) {
            collectBlocks(op, *it);
        }
    };

    if (blockOwner) {
        blockStack.pop();
    };
};

/*
void DataGraphReduction::concatGraphs()
{
//    DataGraphMaster.join();
//    DataGraphMaster.leftOuterJoin();
};
*/

void DataGraphReduction::execute(RPBase* op)
{
    blockStack.push(NULL);
    collectBlocks(NULL, op);

    for (std::vector<DataReductionBlock *>::const_iterator it = blocks.begin(); it != blocks.end(); ++it) {
        DataGraph * dg = (*it)->buildBlock();
        dg->precompile();
        dg->updateIndex();

        rqp::MapConcat * mapConcatIn = dynamic_cast<rqp::MapConcat *>((*it)->in.first);

        if (mapConcatIn != NULL &&
            instanceof<rqp::MapConcat>(mapConcatIn) &&
            mapConcatIn->getSubplan() == (*it)->in.second)
        {
            MapGraph * op = new MapGraph(mapConcatIn->getList(), dg, (*it)->out);
            U_ASSERT(dg->outputNodes.size() == 1);
            DataNode * out = dg->outputNodes[0];
            out->varTupleId = mapConcatIn->getTuple();
            out->varName = mapConcatIn->getContext()->getVarDef(out->varTupleId)->getVarLabel();
            op->tupleMask.insert(out->varTupleId);
            op->getContext()->replaceOperation(mapConcatIn, op);
        } else {
            DataGraphOperation * op = new DataGraphOperation(dg, (*it)->out);
            op->getContext()->replaceOperation((*it)->in.second, op);
        }
    };
}



RPBase* selectDataGraphs(RPBase* op)
{
    DataGraphReduction().execute(op);
    return op;
};
