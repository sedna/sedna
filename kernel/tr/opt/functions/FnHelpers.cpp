#include "FnHelpers.h"

#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/evaluation/VariableMap.h"

using namespace opt;
using namespace rqp;
using namespace phop;


void cleanupFunCall(FunCallParams * funCall)
{
    for (ParamList::iterator it = funCall->paramList.begin(); it != funCall->paramList.end(); ++it) {
        funCall->getContext()->varGraph.getVariable(*it).operations.erase(funCall);
    }
};

