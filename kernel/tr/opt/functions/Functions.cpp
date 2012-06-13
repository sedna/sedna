#include "tr/opt/functions/Functions.h"
#include "tr/executor/base/namespaces.h"

using namespace phop;

FunctionLibrary * phop::functionLibrary = NULL;

FunctionSignature* FunctionLibrary::registerFunction(FunctionSignature* function)
{
    functions.insert(FunctionMap::value_type(function->getName(), function));
    return function;
}

FunctionSignature* FunctionLibrary::findFunction(const xsd::QName& qname)
{
    std::string name = qname.emptyUri() ?
        (std::string("{}:") + qname.getLocalName()) :
        (std::string("{") + qname.getUri() + "}:" + qname.getLocalName());

    FunctionMap::const_iterator it = functions.find(name);

    if (it == functions.end()) {
        U_ASSERT(false);
        return NULL;
    } else {
        return it->second;
    };
}

#define FN_URI (predefinedNamespaces[namespace_fn].uri)

void phop::initializeFunctionLibrary()
{
    if (functionLibrary != NULL) {
        return;
    }

    functionLibrary = new FunctionLibrary();

    functionLibrary->registerFunction(
        new FunctionSignature(FN_URI, "doc", 1))
            ->setFlag(fn_preserves_null);

    functionLibrary->registerFunction(
        new FunctionSignature(FN_URI, "opt_not_empty", 1))
            ->setFlag(fn_preserves_null);
}

