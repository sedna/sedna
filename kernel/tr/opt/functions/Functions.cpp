#include "tr/opt/functions/Functions.h"
#include "tr/executor/base/namespaces.h"

using namespace phop;


FunctionLibrary * phop::functionLibrary = NULL;


void FunctionLibrary::registerFunction(IFunction* function)
{
    functions.insert(FunctionMap::value_type(function->info(), function));
}


#define FN_INFO(PREFIX, NAME)

void fn_doc_op(tuple )
{
};

/*
 fn_doc

class FnDocument : public IFunction {
public:
    FnDocument() {
        m_info.uri = predefinedNamespaces[namespace_fn].uri;
        m_info.name = "doc";
        m_info.hasDataGraph = false;
        m_info.preservesNull = false;
        m_info.rty = 1;
    };
};
*/

void initializeFunctionLibrary()
{
    if (functionLibrary != NULL) {
        return;
    }

    functionLibrary = new FunctionLibrary();
    functionLibrary->registerFunction(new FnDocument());
}

