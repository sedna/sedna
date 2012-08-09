#ifndef _GRAPH_OPERATIONS_H_
#define _GRAPH_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

class MapGraph : public ManyChildren {
    RTTI_DECL(plan_operation_MapGraph, ManyChildren)
protected:
    opt::DataGraphIndex func;
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
private:
    int list_id;
public:
    opt::TupleScheme groupBy;

    void updateVarGraph();

    MapGraph(RPBase* _list, opt::DataGraph * function_, const opt::TupleScheme & _groupBy)
      : ManyChildren(SELF_RTTI_REF, _list), func(function_), groupBy(_groupBy)
    {
        list_id = children.size() - 1;
        function_->operation = this;
        resultChild = list_id;

        updateVarGraph();
    };

    opt::DataGraphIndex & graph() { return func; }
    const opt::DataGraphIndex & graph() const { return func; }

    void joinGraph(opt::DataGraphIndex & dg);
    void leftJoinGraph(opt::DataGraphIndex & dg);

    virtual void evaluateTo(executor::DynamicContext* dynamicContext);

    PROPERTY_RO(List, RPBase *, children[list_id])
};

}

#endif /* _GRAPH_OPERATIONS_H_ */
