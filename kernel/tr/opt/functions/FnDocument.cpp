#include "Functions.h"
#include "FnHelpers.h"

#include "tr/opt/algebra/ElementaryOperations.h"

#include "tr/executor/base/PPUtils.h"

using namespace phop;
using namespace opt;
using namespace rqp;

static
/* Preserves null */
bool rule_fn_doc(PlanRewriter * pr, rqp::FunCall * op)
{
    U_ASSERT(op->children.size() == 1);

    RPBase * arg = op->children[0];

    if (arg == null_op)
    {
        replaceInParent(pr, op, null_op);
        return true;
    };

    if (isConstExpr(arg))
    {
        Const * c = static_cast<Const *>(arg);
        MemoryTupleSequencePtr seq = c->getSequence();

        if (seq->size() == 0)
        {
            replaceInParent(pr, op, null_op);
            return true;
        };

// TODO : resolve uri        std::string uri = PlanContext::current->staticContext()->resolveUri(atomize(seq->at(0)));
        std::string uri = atomize(seq->at(0)).get_str_mem();

        DataGraphBuilder builder;
        DataNode * result = new DataNode(opt::DataNode::dnDatabase);
        result->root = DataRoot(DataRoot::drt_document, uri.c_str());

        builder.nodes.push_back(result);
        builder.out.push_back(result);

        RPBase * newop = new DataGraphOperation(
            builder.build(op->getContext()->dgm()), OperationList());

        replaceInParent(pr, op, newop);

        return true;
    } else if (isGraphExpr(arg)) {
        U_ASSERT(false);
    };
    
    return false;
};

phop::function_info_t doc_function = {rule_fn_doc };
//phop::function_info_t doc_avail_function = {rule_general_comparison_to_graph };
//phop::function_info_t collection_function = {rule_general_comparison_to_graph };

REGISTER_FUNCTIONS_BEGIN(DOC)
    FunctionLibrary * lib = getFunctionLibrary();

    lib->registerFunction(FN_URI, "doc", &doc_function);
//    lib->registerFunction(FN_URI, "doc-available", fn_doc_available_impl);
//    lib->registerFunction(FN_URI, "collection", fn_collection);
REGISTER_FUNCTIONS_END(DOC)
