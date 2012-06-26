#ifndef _GRAPH_OPERATIONS_H_
#define _GRAPH_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {

class DataGraphOperation : public ManyChildren {
    OPERATION(0x01a)
  protected:
    opt::DataGraph * func;

    DataGraphOperation(_opinfo_t op, opt::DataGraph * function_, const OperationList & _oplist)
      : ManyChildren(op, _oplist), func(function_) {
    };

    void detectOutNode();
  public:
    opt::DataNode * out;

    DataGraphOperation(opt::DataGraph * function_, const OperationList & _oplist)
      : ManyChildren(&sopdesc, _oplist), func(function_), out(NULL)
    {
        detectOutNode();
    };

    PROPERTY(Graph, opt::DataGraph *, func)
//    PROPERTY(Mapping, const opt::TupleMapping &, tmapping)
};

class MapGraph : public DataGraphOperation {
    OPERATION(0x01b)
private:
    int list_id;
public:
    MapGraph(RPBase* _list, opt::DataGraph * function_, const OperationList & _oplist)
      : DataGraphOperation(&sopdesc, function_, _oplist) {
        list_id = children.size();
        children.push_back(_list);
        context->registerLink(this, _list, &(children[list_id]));
    };

    opt::TupleScheme tupleMask;

    PROPERTY_RO(List, RPBase *, children[list_id])
};

}

#endif /* _GRAPH_OPERATIONS_H_ */
