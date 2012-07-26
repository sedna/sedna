#include "Functions.h"
#include "FnHelpers.h"

#include "tr/opt/algebra/ElementaryOperations.h"
#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/graphs/Predicates.h"
#include "tr/executor/base/PPUtils.h"

using namespace phop;
using namespace opt;
using namespace rqp;

/* Preserves null */

static
bool rule_fn_doc(PlanRewriter * pr, rqp::FunCall * op)
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
    DataNode * result = new DataNode(opt::DataNode::dnDatabase, optimizer->context()->generateTupleId());
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

phop::function_info_t doc_function = {rule_fn_doc };
//phop::function_info_t doc_avail_function = {rule_general_comparison_to_graph };
//phop::function_info_t collection_function = {rule_general_comparison_to_graph };

REGISTER_FUNCTIONS_BEGIN(DOC)
    FunctionLibrary * lib = getFunctionLibrary();

    lib->registerFunction(FN_NS.prefix, FN_NS.uri, "doc", &doc_function);
//    lib->registerFunction(FN_URI, "doc-available", fn_doc_available_impl);
//    lib->registerFunction(FN_URI, "collection", fn_collection);
REGISTER_FUNCTIONS_END(DOC)
