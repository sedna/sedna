#include "PlanRewriter.h"

#include "tr/opt/OptTypes.h"

#include "tr/opt/algebra/AllOperations.h"

#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/Predicates.h"

#include "tr/opt/functions/Functions.h"

#include "tr/opt/graphs/GraphRewriter.h"

#include "tr/debugstream.h"

#include <vector>
#include <map>
#include <stack>

#include <boost/shared_ptr.hpp>

using namespace opt;
using namespace rqp;

struct DataReductionBlock {
    RPEdge in;
    OperationList out;
    OperationList members;

    DataGraph* buildBlock();
};

struct GraphContext;

typedef boost::shared_ptr<GraphContext> GraphContextPtr;

struct GraphContext {
    GraphContextPtr parent;
    counted_ptr<DataGraphWrapper> graph;
    TupleScheme visibleVariables;
    bool breakNullPreserve;
    RPBase * op;

    explicit GraphContext(GraphContextPtr aparent, bool _breakNullPreserve, DataGraph * _graph)
        : parent(aparent), graph(NULL), breakNullPreserve(_breakNullPreserve)
    {
        if (parent.get() != NULL) {
            visibleVariables.insert(parent->visibleVariables.begin(), parent->visibleVariables.end());
        }

        if (_graph != NULL) {
            graph = new DataGraphWrapper(_graph);
            visibleVariables.insert(graph->outTuples.begin(), graph->outTuples.end());
        }
    };

    ~GraphContext() {};
};

class DataGraphReduction {
    std::vector<DataReductionBlock *> blocks;
    std::map<RPBase *, DataReductionBlock *> opBlockMap;
    std::stack<DataReductionBlock *> blockStack;

    OperationList opStack;
    GraphContextPtr dataGraphStack;

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

    DataGraphReduction & execute(RPBase * op);

    void findJoinsRec(RPBase* op);
    bool tryJoin(DataGraphWrapper & left, DataGraphWrapper & right);
    void selectPossibleJoins(DataGraphWrapper& right, RPBase* op, RPBase* substOp, TupleId resultTuple);
};

RPBase* selectDataGraphs(RPBase* op)
{
    DataGraphReduction().execute(op);
    return op;
};


/*
RPBase* varUsageAnalyzis(RPBase* op)
{
    OptimizationContext context;



    return op;
}
*/

typedef std::map<RPBase *, DataNode *> ResultMap;
typedef std::multimap<opt::TupleId, opt::TupleId> ExternalMap;

struct Generator {
    ResultMap map;
    DataGraphBuilder builder;

    DataNode * get(RPBase * op) {
        DataNode * x = NULL;

        if (map.find(op) == map.end()) {
            x = new DataNode(opt::DataNode::dnExternal);
            builder.nodes.push_back(x);
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
    DataNode * node;

    rqp::MapConcat * mapConcatIn = dynamic_cast<rqp::MapConcat *>(in.first);
    
    if (mapConcatIn != NULL && mapConcatIn->getSubplan() != in.second) {
        mapConcatIn = NULL;
    }
    
    for (OperationList::const_iterator it = members.begin(); it != members.end(); ++it) {
        RPBase * op = *it;

        switch (op->info()->opType) {
          case rqp::Select::opid : {
            U_ASSERT(dynamic_cast<rqp::Select *>(op) != NULL);
            rqp::Select * sop = static_cast<rqp::Select *>(op);

            node = context.get(op);
            node->type = DataNode::dnAlias;
            node->aliasFor = context.get(sop->getList());
            node->varTupleId = sop->getContextTuple();
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
            node->varTupleId = vop->getTuple();

            dgm->setVarName(node->varTupleId, varName(vop));
          } break;
          case rqp::XPathStep::opid : {
            U_ASSERT(dynamic_cast<rqp::XPathStep *>(op) != NULL);

            rqp::XPathStep * xop = static_cast<rqp::XPathStep *>(op);
            node = context.get(op);
            node->type = DataNode::dnFreeNode;

            context.builder.predicates.push_back(
                new StructuralPredicate(context.get(xop->getList()), node, xop->getStep()));
          } break;
/*
          case rqp::ComparisonExpression::opid : {
            U_ASSERT(dynamic_cast<rqp::ComparisonExpression *>(op) != NULL);

            rqp::ComparisonExpression * cop = static_cast<rqp::ComparisonExpression *>(op);
            
            node = context.get(op);
            node->type = DataNode::dnConst;

            node->sequence = new MemoryTupleSequence();
            node->sequence->push_back(tuple_cell::atomic(true));

            context.builder.predicates.push_back(
                new ValuePredicate(context.get(cop->getLeft()), context.get(cop->getRight()), cop->getOp()));
          } break;
*/
          case rqp::FunCall::opid : {
            U_ASSERT(dynamic_cast<rqp::FunCall *>(op) != NULL);

            rqp::FunCall * cop = static_cast<rqp::FunCall *>(op);

            node = context.get(op);
            node->type = DataNode::dnFreeNode;

            // TODO: Fix number of parameters
            context.builder.predicates.push_back(
                new FPredicate(context.get(cop->children.at(0)), node, NULL));
          } break;
          default:
            U_ASSERT(false);
          break;
        };
    };

    DataNode * out = context.get(in.second);
    
    if (mapConcatIn != NULL) {
        node = context.get(mapConcatIn);

        node->type = DataNode::dnAlias;
        node->aliasFor = out;
        node->varTupleId = mapConcatIn->context.item;

        if (out->varTupleId == invalidTupleId) {
            out->varTupleId = node->varTupleId;
        };
        
        dgm->setVarName(node->varTupleId, PlanContext::current->getVarDef(node->varTupleId)->getVarLabel());
    } else {
        if (out->varTupleId == invalidTupleId) {
            out->varTupleId = PlanContext::current->generateTupleId();
        }
    };

    context.builder.out.push_back(out);

    DataGraph * dg = context.builder.build(dgm);
    DataNodeList * nodeList = &(context.builder.nodes);

    for (DataNodeList::const_iterator it = nodeList->begin(); it != nodeList->end(); ++it) {
        if ((*it)->varTupleId != opt::invalidTupleId) {
            dgm->addVariable(*it);
        }
    }

    return dg;
}

void DataGraphReduction::collectBlocks(RPBase * parent, RPBase * op)
{
    bool blockOwner = false;
    bool blockBuilder = (op->info()->flags & rqp::oBlockBuilder) > 0;

    OperationList & children = op->children;

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

DataGraphReduction & DataGraphReduction::execute(RPBase* op)
{
    blockStack.push(NULL);
    collectBlocks(NULL, op);

    for (std::vector<DataReductionBlock *>::const_iterator it = blocks.begin(); it != blocks.end(); ++it) {
        if ((*it)->members.size() > 1) {
            DataGraph * dg = (*it)->buildBlock();

            DataGraphRewriter dgrw(dg);

            dgrw.aliasResolution();
            dgrw.selfReferenceResolution();
            dgrw.structuralComparison();
            dgrw.doPathExpansion();

            rqp::MapConcat * mapConcatIn = dynamic_cast<rqp::MapConcat *>((*it)->in.first);

            if (mapConcatIn != NULL && instanceof<rqp::MapConcat>(mapConcatIn) &&
                mapConcatIn->getSubplan() == (*it)->in.second)
            {
                MapGraph * op = new MapGraph(mapConcatIn->getList(), dg, (*it)->out);
                op->tupleMask.insert(mapConcatIn->getTuple());
                op->getContext()->replaceOperation(mapConcatIn, op);
            } else {
                DataGraphOperation * op = new DataGraphOperation(dg, (*it)->out);
                op->getContext()->replaceOperation((*it)->in.second, op);
            }
        }
    };

    findJoinsRec(op);
    
    return *this;
}

// TODO : to the library

template<class Set1, class Set2>
static
size_t intersection_size(const Set1 &set1, const Set2 &set2)
{
    if(set1.empty() || set2.empty()) return 0;

    typename Set1::const_iterator it1 = set1.begin(), it1End = set1.end();
    typename Set2::const_iterator it2 = set2.begin(), it2End = set2.end();

    if (*it1 > *set2.rbegin() || *it2 > *set1.rbegin()) return 0;

    size_t result = 0;

    while(it1 != it1End && it2 != it2End)
    {
        if (*it1 == *it2) {
            result++;
            it1++;
            it2++;
        }

        if (*it1 < *it2) {
            it1++;
        } else {
            it2++;
        }
    }

    return result;
}

bool DataGraphReduction::tryJoin(DataGraphWrapper& left, DataGraphWrapper& right)
{
    DataGraphMaster * dgm = PlanContext::current->dgm();
    
    size_t linkSize = intersection_size(left.outTuples, right.inTuples);
    size_t totalSize = left.nodes.size() + right.nodes.size();

    // TODO: calculate weights
    if (linkSize > 0 && totalSize < 40) {
        dgm->join(left.dg, right.dg);

        DataGraphRewriter dgrw(left.dg);

        dgrw.selfReferenceResolution();
        dgrw.doPathExpansion();

        left.update();

        return true;
    };

    return false;
}

void DataGraphReduction::selectPossibleJoins(DataGraphWrapper& dgw, RPBase* op, RPBase* substOp, TupleId resultTuple)
{
    if (dataGraphStack.get() == NULL) {
        return;
    };

    GraphContextPtr it = dataGraphStack;

    {
        std::stringstream ss;

        while (it.get() != NULL) {
            if (!it->graph.isnull()) {
                ss << (intptr_t) it->graph->dg << " ";
            } else {
                ss << "[break by " << it->op->info()->opname << "] ";
            };

            it = it->parent;
        };

        ss << "\n";

        debug_string("opt.o", ss.str());

        it = dataGraphStack;
    }
    
    while (it.get() != NULL) {
        if (it->breakNullPreserve) {
            break;
        };

        if (!it->graph.isnull()) {
            // TODO : make the best candidate selection
            if (tryJoin(*it->graph, dgw)) {
                if (substOp == null_op && resultTuple != invalidTupleId) {
                    substOp = new rqp::VarIn(resultTuple);
                };

                PlanContext::current->replaceOperation(op, substOp);

                break;
            };
        };

        it = it->parent;
    };
};

void DataGraphReduction::findJoinsRec(RPBase* op)
{
    U_ASSERT(op != NULL);
    opStack.push_back(op);

    if (instanceof<rqp::MapGraph>(op)) {
        rqp::MapGraph * mapG = dynamic_cast<rqp::MapGraph *>(op);

        dataGraphStack.reset(new GraphContext(dataGraphStack, false, mapG->getGraph()));

        if (mapG->getList() != null_op) {
            findJoinsRec(mapG->getList());
        }

        dataGraphStack = dataGraphStack->parent;

        DataGraphWrapper dgw(mapG->getGraph());
        
        selectPossibleJoins(dgw, mapG, mapG->getList(), invalidTupleId);
    } else if (instanceof<rqp::DataGraphOperation>(op)) {
        rqp::DataGraphOperation * dgop = dynamic_cast<rqp::DataGraphOperation *>(op);
        DataGraphWrapper dgw(dgop->getGraph());

        U_ASSERT(dgw.outTuples.size() == 1);
        TupleId resultTuple = *dgw.outTuples.begin();

        U_ASSERT(dgop->getOperations().empty());
        selectPossibleJoins(dgw, dgop, null_op, resultTuple);
    } else {
        bool preservesNull = true;

        switch (op->info()->opType) {
          case rqp::If::opid : {
              preservesNull = (static_cast<rqp::If *>(op)->getElse() == null_op);
          } break;
          case rqp::Construct::opid :
              preservesNull = false;
          break;
          default:
             //
          break;
        };

        if (!preservesNull) {
            dataGraphStack.reset(new GraphContext(dataGraphStack, true, NULL));
            dataGraphStack->op = op;
        }
        
        OperationList children;
        op->getChildren(children);
        for (OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
            if (*it != NULL) {
                findJoinsRec(*it);
            }
        };
        
        if (!preservesNull) {
            dataGraphStack = dataGraphStack->parent;
        }      
    }
    
    opStack.pop_back();
};  

