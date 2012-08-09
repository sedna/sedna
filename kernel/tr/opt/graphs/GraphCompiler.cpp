#include "GraphCompiler.h"
#include "GraphRewriter.h"

#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/opt/phm/Operations.h"

// ***************************** Data Graph ***************************
/*
#include <iostream>
#include <fstream>

#include "tr/models/XmlConstructor.h"

#include "tr/crmutils/serialization.h"
#include "tr/crmutils/exec_output.h"
*/

using namespace opt;
using namespace phop;

// ***************************** Execution Plan ***************************

class PlanMap {
private:
    typedef std::map<PlanDesc, PlanInfo *> PlanInfoMap;
    PlanInfoMap planInfoMap;
    PlanInfo * last;
public:
    PlanInfo * get(PlanDesc i) const { return planInfoMap.at(i); };

    bool exists(PlanDesc i) const {
        return planInfoMap.find(i) != planInfoMap.end();
    };

    PlanInfo * getLastPlan() { return last; };

    PlanInfo * update(PlanInfo * _plan) {
        PlanDesc desc = _plan->getDesc();
        PlanInfoMap::iterator i = planInfoMap.find(desc);

        if (i == planInfoMap.end()) {
            planInfoMap.insert(PlanInfoMap::value_type(desc, _plan));
            last = _plan;
        } else if (i->second->getTotalCost() > _plan->getTotalCost()) {
            i->second = _plan;
            last = _plan;
        } else {
            last = i->second;
        }

        return last;
    };
};

phop::GraphExecutionBlock* opt::GraphCompiler::compile(DataGraphIndex& graph)
{
    opt::DataGraphRewriter dgr(graph);
    dgr.structuralComparison();
    dgr.expandAbsolutePath();
  
    phop::GraphExecutionBlock* result = getGraph(graph.dg);
    
    if (result != NULL) {
        return result;
    };

    result = new GraphExecutionBlock();

    GraphExecutionBlock::push(result);

/*
    std::ofstream F("/tmp/datagraph.log");
    se_stdlib_ostream Fstream(F);

    SCElementProducer * vrt = SCElementProducer::getVirtualRoot(XNULL);
    GlobalSerializationOptions opt;

    opt.indent = true;
    opt.indentSequence = "  ";
    opt.separateTuples = true;
    opt.useCharmap = true;

    Serializer * serializer = Serializer::createSerializer(se_output_method_xml);
    serializer->prepare(&Fstream, &opt);
*/

    PlanMap * planMap = new PlanMap();

    PlanDescSet set1, set2;

    PlanDescSet * currentStepSet = &set1;
    PlanDescSet * nextStepSet = &set2;

    PlanInfo * nullPlan = new PlanInfo(graph.nodes.size());

    {
        PhysicalModel nullPlanPhm(nullPlan);

        int branchLimit = 4;

        FOR_ALL_GRAPH_ELEMENTS(graph.dg->dataNodes, i) {
            DataNode * dn = graph.dg->dataNodes[i];
            TupleRef ref = nullPlan->initTupleSet(dn);

            if (dn->type == opt::DataNode::dnExternal) {
                nullPlanPhm.pushOp(
                  new ExternalVarPrototype(&nullPlanPhm, ref));
            };
        };
    }

    planMap->update(nullPlan);
    currentStepSet->insert(0);

    int branchLimit = 3;
    
    while (!currentStepSet->empty()) {
        nextStepSet->clear();

/*
        F << "\n Next set : ";

        for (PlanDescSet::const_iterator it = currentStepSet->begin(); it != currentStepSet->end(); ++it) {
            F << *it << " ";
        }

        F << "\n\n";
*/

        for (PlanDescSet::const_iterator it = currentStepSet->begin(); it != currentStepSet->end(); ++it) {
            PlanInfo * info = planMap->get(*it);
            PlanDesc dsc = graph.getNeighbours(info->getDesc());

//            if (dsc == 0 && branchLimit > 0) {
            if (branchLimit > 0) {
                branchLimit--;
                dsc = graph.predicateMask & ~info->getDesc();
            };

            PlanDescIterator neighbours(dsc);

            int nei;
            while (-1 != (nei = neighbours.next())) {
                PlanInfo * candidate = info->extend(graph.dg->predicates[nei]);

                if (candidate != NULL) {
                    candidate = planMap->update(candidate);
                    nextStepSet->insert(candidate->getDesc());

/*
                    serializer->serialize(tuple(candidate->toXML(vrt)->close()));

                    F << "\n--------------\n";
                    F.flush();
*/
                }
            };
        };

//        F << "\n============\n";

        PlanDescSet * swapset = currentStepSet;
        currentStepSet = nextStepSet;
        nextStepSet = swapset;
    };

//    serializer->serialize(tuple(planMap->getLastPlan()->toXML(vrt)->close()));

    ITupleOperator * checkVar = planMap->getLastPlan()->compile();
    U_ASSERT(checkVar == result->top());
    graphCache[graph.dg] = result;

    return result;
}

