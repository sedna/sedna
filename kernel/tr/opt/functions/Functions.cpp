#include "tr/opt/functions/Functions.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/executor/base/namespaces.h"
#include "auxiliary/cppcast.h"

using namespace phop;

FunctionLibrary * phop::functionLibrary = NULL;

void FunctionInfo::execute(rqp::FunCallParams* funcall, executor::DynamicContext* dynamicContext)
{
    throw USER_EXCEPTION2(0, "This function is not implemented in algebra mode. This is an optimizer error.");
}


void phop::initFunctionLibrary()
{
    phop::functionLibrary = new FunctionLibrary;
}

FunctionLibrary::FunctionLibrary()
{

}

FunctionLibrary::~FunctionLibrary()
{
    for (FunctionMap::const_iterator it = functions.begin(); it != functions.end(); ++it)
    {
        delete it->second;
    };
}

static
std::string getSearchName(const char* uri, const char* localname, int argc)
{
    return cast_to_string<int>(argc) + "/" + localname + ((uri == NULL) ? "@{}" : ("@{" + std::string(uri) + "}"));
};

FunctionInfo* FunctionLibrary::registerFunction(FunctionInfo * finfo)
{
    std::string name = getSearchName(finfo->finfo->uri, finfo->finfo->localname, finfo->finfo->argc);
    functions.insert(FunctionMap::value_type(name, finfo));
    return finfo;
}

FunctionInfo* FunctionLibrary::findFunction(const xsd::QName& qname, int argc)
{
    std::string name = getSearchName(qname.getUri(), qname.getLocalName(), argc);

    FunctionMap::const_iterator it = functions.find(name);

    if (it == functions.end()) {
        return NULL;
    } else {
        return it->second;
    };
}


