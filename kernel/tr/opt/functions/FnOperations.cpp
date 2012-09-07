#include "FnOperations.h"
#include "FnHelpers.h"
#include "TypeChecker.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/evaluation/DynamicContext.h"

using namespace rqp;
using namespace opt;

class BinaryTupleOperation : public phop::FunctionInfo
{
    static phop::function_info_t op_name;
public:
    BinaryTupleOperation() : FunctionInfo(&op_name) {};
    BinaryTupleOperation(const phop::function_info_t * f) : FunctionInfo(f) {};

    virtual bool transform(FunCallParams* funcall, RewritingContext* p);
};

static const error_info_t cast_error = {XPTY0004, "Wrong arguments in operator"};

phop::function_info_t
BinaryTupleOperation::op_name = {NULL, "internal", "binary_tuple_operation", 2};

bool BinaryTupleOperation::transform(FunCallParams* funcall, RewritingContext* p)
{
    BinaryTupleCellOpData * data = static_cast<BinaryTupleCellOpData *>(funcall->getData());

    TupleId arg1 = funcall->getParams().at(0);
    TupleId arg2 = funcall->getParams().at(1);

    TupleInfo & arg1Info = funcall->getContext()->varGraph.getVariable(arg1);
    TupleInfo & arg2Info = funcall->getContext()->varGraph.getVariable(arg2);

    rqp::RPBase * arg1Producer = getProducer(arg1Info.definedIn);
    rqp::RPBase * arg2Producer = getProducer(arg2Info.definedIn);

    /* Evaluate constant expressions */
    if (isConstExpr(arg1Producer) && isConstExpr(arg2Producer)) {
        tuple_cell arg1Value;
        tuple_cell arg2Value;

        if (arg1Producer != null_op) {
            MemoryTupleSequencePtr seq = static_cast<rqp::Const *>(arg1Producer)->getSequence();

            arg1Value = TypedSequenceIterator<MemoryTuplesWrapper, TypeAtomizer>(
                MemoryTuplesWrapper(seq->begin(), seq->end()), TypeAtomizer(), 0, -1, cast_error).next();
        };

        if (arg2Producer != null_op) {
            MemoryTupleSequencePtr seq = static_cast<rqp::Const *>(arg2Producer)->getSequence();

            arg2Value = TypedSequenceIterator<MemoryTuplesWrapper, TypeAtomizer>(
                MemoryTuplesWrapper(seq->begin(), seq->end()),
                TypeAtomizer(), 0, -1, cast_error).next();
        }

        rqp::Const * value;

        if (data->collation == NULL) {
            value = new rqp::Const(data->bf(arg1Value, arg2Value));
        } else {
            value = new rqp::Const(data->bfc(arg1Value, arg2Value, data->collation));
        };

        cleanupFunCall(funcall);
        p->replaceInParent(funcall, value);

        return true;
    };

    return false;

/*
    DataGraphIndex builder(new DataGraph(&funcall->getContext()->varGraph));

    DataNode * left = new DataNode(DataNode::dnExternal, funcall->getParams().at(0));
    DataNode * right = new DataNode(DataNode::dnExternal, funcall->getParams().at(1));

    builder.nodes.push_back(left);
    builder.nodes.push_back(right);
    builder.predicates.push_back(new ValuePredicate(left, right, data->cmp));

    builder.rebuild();

    RPBase * newOp = new MapGraph(
      new Const(funcall->getContext()->varGraph.alwaysTrueSequence),
      builder.dg, TupleScheme());

    p->replaceInParent(funcall, newOp);

    cleanupFunCall(funcall);

    return true;
*/
}

void BinaryTupleCellOpData::init(xmlscm_type t1, xmlscm_type t2)
{
    get_binary_op_res op = get_binary_op(ftype, t1, t2);

    bf = op.f.bf;
    bfc = op.f.bf_c;

    if (op.collation && collation == NULL) {
        throw USER_EXCEPTION2(XPTY0004, "Errornous collation in static context");
    };
}

XmlConstructor& BinaryTupleCellOpData::toXML(XmlConstructor& constructor) const
{
    constructor.addElementValue(SE_EL_NAME("operation"), xq_binary_op_type2string(ftype));
    return constructor;
}

using namespace phop;

FunctionInfo * binaryOperationFunction = NULL;

REGISTER_FUNCTIONS_BEGIN(BINOP)
    FunctionLibrary * lib = getFunctionLibrary();
    binaryOperationFunction = lib->registerFunction(new BinaryTupleOperation());
REGISTER_FUNCTIONS_END(BINOP)
