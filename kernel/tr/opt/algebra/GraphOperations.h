#ifndef _GRAPH_OPERATIONS_H_
#define _GRAPH_OPERATIONS_H_

#include "IndependentPlan.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

class DataGraphOperation : public ManyChildren {
    RTTI_DECL(op_datagraph, ManyChildren)
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
protected:
    opt::DataGraphIndex func;

    DataGraphOperation(clsinfo_t op, opt::DataGraph * function_, const OperationList & _oplist)
      : ManyChildren(op, _oplist), func(function_) {
    };

    void detectOutNode();
public:
    opt::DataNode * out;

    DataGraphOperation(opt::DataGraph * function_)
      : ManyChildren(SELF_RTTI_REF), func(function_), out(NULL)
    {
        detectOutNode();
    };

    DataGraphOperation(opt::DataGraph * function_, const OperationList & _oplist)
      : ManyChildren(SELF_RTTI_REF, _oplist), func(function_), out(NULL)
    {
        detectOutNode();
    };

    opt::DataGraphIndex & graph() { return func; }
    const opt::DataGraphIndex & graph() const { return func; }
};

class MapGraph : public DataGraphOperation {
    RTTI_DECL(op_map_graph, DataGraphOperation)
protected:
    virtual XmlConstructor& __toXML ( XmlConstructor& constructor ) const;
private:
    int list_id;
public:
    MapGraph(RPBase* _list, opt::DataGraph * function_, const OperationList & _oplist)
      : DataGraphOperation(SELF_RTTI_REF, function_, _oplist) {
        list_id = children.size();
        children.push_back(_list);
        resultChild = list_id;
    };

    opt::TupleScheme tupleMask;

    void joinGraph(opt::DataGraphIndex & dg);
    void leftJoinGraph(opt::DataGraphIndex & dg);

    PROPERTY_RO(List, RPBase *, children[list_id])
};

}

#endif /* _GRAPH_OPERATIONS_H_ */
