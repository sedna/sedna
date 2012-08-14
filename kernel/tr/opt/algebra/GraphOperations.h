#ifndef _GRAPH_OPERATIONS_H_
#define _GRAPH_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

class MapGraph : public ListOperation {
    RTTI_DECL(plan_operation_MapGraph, ListOperation)
protected:
    opt::DataGraphIndex func;
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
public:
    opt::TupleScheme groupBy;

    void updateVarGraph();

    MapGraph(RPBase* _list, opt::DataGraph * function_, const opt::TupleScheme & _groupBy)
      : ListOperation(SELF_RTTI_REF, _list), func(function_), groupBy(_groupBy)
    {
        function_->operation = this;
        updateVarGraph();
    };

    opt::DataGraphIndex & graph() { return func; }
    const opt::DataGraphIndex & graph() const { return func; }

    void joinGraph(opt::DataGraphIndex & dg);
    void leftJoinGraph(opt::DataGraphIndex & dg);

    virtual void evaluateTo(executor::DynamicContext* dynamicContext);
};

}

#endif /* _GRAPH_OPERATIONS_H_ */
