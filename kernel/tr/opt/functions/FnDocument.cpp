#include "Functions.h"

#include "FnHelpers.h"
#include "TypeChecker.h"

#include "tr/opt/algebra/ElementaryOperations.h"
#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/evaluation/DynamicContext.h"
#include "tr/opt/evaluation/VariableMap.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/updates/bulkload.h"

using namespace phop;
using namespace opt;
using namespace rqp;

class LoadDocumentFunction : public phop::FunctionInfo
{
public:
    static phop::function_info_t name;

    LoadDocumentFunction(const phop::function_info_t * f) : FunctionInfo(f) {
        isUpdate = true;
    };

    virtual void execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext);
};

class DocumentFunction : public phop::FunctionInfo
{
public:
    static phop::function_info_t doc_name;
    DocumentFunction(const phop::function_info_t * f) : FunctionInfo(f) {};

    virtual void execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext);
    virtual bool transform(FunCallParams* funcall, RewritingContext* p);
};

phop::function_info_t
DocumentFunction::doc_name = {FN_NS.prefix, FN_NS.uri, "doc", 1};

phop::function_info_t
LoadDocumentFunction::name = {SE_NS.prefix, SE_NS.uri, "load-doc", 2};

void DocumentFunction::execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
    U_ASSERT(false);
//    throw ;
}

using executor::SequenceIterator;

executor::SequenceIterator getArg(FunCallParams* funcall, executor::DynamicContext* dynamicContext, int n)
{
    return dynamicContext->variables->getIterator(funcall->getParams().at(n));
};

#define GET_ARG(N) dynamicContext->variables->getIterator(funcall->getParams().at(N))

struct LoadDocumentExecutor : public executor::IExecuteProc
{
    tuple_cell fileName;
    tuple_cell docName;

    virtual void execute(executor::ResultSequence * sequence);

    LoadDocumentExecutor(const tuple_cell & _fileName, const tuple_cell & _docName)
      : fileName(_fileName), docName(_docName) {};
};

void LoadDocumentExecutor::execute(executor::ResultSequence* sequence)
{
    BulkLoadFrontend bulkLoadManager;

    std::vector<std::string> filenames(1, fileName.get_str_mem());
    std::vector<client_file> cf_vec(1);
    tr_globals::client->get_file_from_client(&filenames, &cf_vec);

    bulkLoadManager.setSourceStream(cf_vec.at(0).stream);
    bulkLoadManager.loadDocument(docName.get_str_mem());
}

void LoadDocumentFunction::execute(FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
    error_info_t cast_error = {XPTY0004, "Wrong arguments in function se:load-doc"};

    tuple_cell fileName =
      TypedSequenceIterator<SequenceIterator, AtomicTypeCaster>
        (GET_ARG(0), AtomicTypeCaster(xs_string), 1, 1, cast_error).next();

    tuple_cell docName =
      TypedSequenceIterator<SequenceIterator, AtomicTypeCaster>
        (GET_ARG(1), AtomicTypeCaster(xs_string), 1, 1, cast_error).next();

    dynamicContext->updates->push(executor::Result(new LoadDocumentExecutor(fileName, docName)));
};

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

REGISTER_FUNCTIONS_BEGIN(DOC)
    FunctionLibrary * lib = getFunctionLibrary();

    lib->registerFunction(new DocumentFunction(&DocumentFunction::doc_name));
    lib->registerFunction(new LoadDocumentFunction(&LoadDocumentFunction::name));
REGISTER_FUNCTIONS_END(DOC)
