#include "Functions.h"
#include "FnHelpers.h"

#include "tr/opt/algebra/ElementaryOperations.h"
#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/evaluation/DynamicContext.h"
#include "tr/executor/base/PPUtils.h"

using namespace phop;
using namespace opt;
using namespace rqp;

class DocumentFunction : public phop::FunctionInfo
{
public:
    static phop::function_info_t doc_name;

    DocumentFunction(const phop::function_info_t * f) : FunctionInfo(f) {};

    virtual void execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext);
    virtual bool transform(FunCallParams* funcall, RewritingContext* p);
};

phop::function_info_t
DocumentFunction::doc_name = {FN_NS.prefix, FN_NS.uri, "doc"};

void DocumentFunction::execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
//    dynamicContext->variables;
}


bool DocumentFunction::transform(FunCallParams* funcall, RewritingContext* p)
{
    TupleId result = funcall->getContext()->generateTupleId();

    DataGraphIndex builder(new DataGraph(&funcall->getContext()->varGraph));

    DataNode * left = new DataNode(DataNode::dnExternal, funcall->getParams().at(0));
    DataNode * right = new DataNode(DataNode::dnFreeNode, result);

    builder.nodes.push_back(left);
    builder.addOutNode(right);
    builder.predicates.push_back(new FnDocPredicate(left, right));

    builder.rebuild();

    RPBase * newOp = new MapGraph(
      new VarIn(result), builder.dg, TupleScheme());

    p->replaceInParent(funcall, newOp);

    cleanupFunCall(funcall);

    return true;
}

/*
static
bool rule_fn_doc(RewritingContext * pr, rqp::FunCall * op)
{
    U_ASSERT(op->children.size() == 1);

    RPBase * arg = op->children[0];

    if (arg == null_obj)
    {
        pr->replaceInParent(op, null_op);
        return true;
    };

    if (!instanceof<Const>(arg)) {
        U_ASSERT(false);
    };

    DataGraphIndex builder(new DataGraph(optimizer->dgm()));
//    DataNode * left = addGraphToJoin(builder, arg);
//    DataNode * result = new DataNode(opt::DataNode::dnFreeNode, optimizer->context()->generateTupleId());
    DataNode * result = new DataNode(opt::DataNode::dnDatabase, optimizer->planContext()->generateTupleId());
    tuple_cell name = static_cast<Const *>(arg)->getSequence()->at(0);
    result->root = DataRoot(DataRoot::drt_document, atomize(name).get_str_mem());

    optimizer->dgm()->addVariable(result);

//    builder.nodes.push_back(left);
    builder.addOutNode(result);
    builder.rebuild();

    RPBase * newop =
      new MapGraph(
        new VarIn(result->varTupleId), builder.dg, TupleScheme());

    pr->replaceInParent(op, newop);

    return true;
};
*/

REGISTER_FUNCTIONS_BEGIN(DOC)
    FunctionLibrary * lib = getFunctionLibrary();

    lib->registerFunction(new DocumentFunction(&DocumentFunction::doc_name));
//    lib->registerFunction(FN_URI, "doc-available", fn_doc_available_impl);
//    lib->registerFunction(FN_URI, "collection", fn_collection);
REGISTER_FUNCTIONS_END(DOC)
