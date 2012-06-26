#include "tr/opt/functions/Functions.h"
#include "tr/opt/SequenceModel.h"
#include "tr/executor/base/namespaces.h"

using namespace phop;

FunctionLibrary * phop::functionLibrary = NULL;

/*
class Compare {
};

tuple_cell f1(const tuple & x) {
};

tuple_cell f2_next(const tuple & x) {
};

tuple_cell f2_reset(const tuple & x) {
};

tuple_cell f1(const tuple & x) {
};

class Function : public ITupleOperator
{
};

class Comparison : public ITupleOperator
{
private:
    virtual void do_next();
public:
    virtual void reset();
};

struct function_data_t
{
    lazy_impl ;
    apply ;
};

bool apply_equals(void * data, opt::DataGraph * dg)
{
    
};







*/







FunctionInfo* FunctionLibrary::registerFunction(FunctionInfo* function)
{
    functions.insert(FunctionMap::value_type(function->getName(), function));
    return function;
}

FunctionInfo* FunctionLibrary::findFunction(const xsd::QName& qname)
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
        new FunctionInfo(FN_URI, "doc", 1))
            ->setFlag(fn_preserves_null);

    functionLibrary->registerFunction(
        new FunctionInfo(FN_URI, "opt_not_empty", 1))
            ->setFlag(fn_preserves_null);
}

