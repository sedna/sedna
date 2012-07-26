#include "GraphOperations.h"

#include "tr/opt/graphs/DataGraphs.h"
#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/algorithms/ExecutionContext.h"
#include "tr/opt/algorithms/SequenceModel.h"

using namespace rqp;
using namespace opt;

RTTI_DEF(MapGraph)

XmlConstructor& MapGraph::__toXML(XmlConstructor& element) const
{
    for (TupleScheme::const_iterator it = groupBy.begin(); it != groupBy.end(); ++it) {
        element.openElement(SE_EL_NAME("group-by"));
        element.addAttributeValue(SE_EL_NAME("tid"), tuple_cell::atomic_int(*it));
        element.closeElement();
    };

    graph().dg->toXML(element);

    if (getList() != null_obj) {
        getList()->toXML(element);
    }

    return element;
};

void MapGraph::joinGraph(DataGraphIndex& rg)
{
    func.nodes.insert(func.nodes.end(), rg.nodes.begin(), rg.nodes.end());
    func.predicates.insert(func.predicates.end(), rg.predicates.begin(), rg.predicates.end());
    func.out.insert(func.out.end(), rg.out.begin(), rg.out.end());

    func.rebuild();
}
